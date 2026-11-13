# ADR 0002: Cache The Analysis Result Of Each File

## Status

Proposed, 2026-11-12

## Context

A run of cljs-patrol has these phases:

1. Discovery finds the source files.
2. For each file, the parser reads the file, and the group handlers walk its nodes. The result of one file is a map
   `{:declarations :usages :dynamic-sites}` (`analyze-file` in `parser.clj`).
3. `analyze-project` joins the file results in file order. Then each group runs its cross-file `analyze` step on the
   joined data.
4. The baseline comparison and the report use the findings.

`--timings` shows that step 2 uses approximately 98% of a run. Steps 3 and 4 use approximately 2%.

In CI, most runs change only a small number of files. But each run reads and walks all files again. Parallel analysis
(merge commit `cd79eeb551`) does not help much on CI runners that have 1 physical core (see
[ADR 0001](0001-native-image-garbage-collector.md)). Thus the remaining large speed-up is to not analyze the files that
did not change.

A research study on a large codebase found these facts:

- **The file result is plain data.** It contains maps, vectors, strings, keywords, symbols and numbers. It contains no
  zippers, nodes, functions, atoms or regexes. A run that joins file results after an EDN write and read gives output
  that is identical to a normal run, for the CI groups and for all groups.
- **Some keywords and symbols do not survive an EDN write and read.** The parser makes them from string requires, for
  example `["@sentry/react" :as sentry]`. The namespace of the keyword is the raw string.
  - `:@sentry/react/captureException` gives a read error.
  - `:react-dom/client/createRoot` gives a different keyword and no error. The namespace becomes `react-dom` and the
    name becomes `client/createRoot`.

  Thus a read error is not a sufficient check. These keywords and symbols need a fix before a cache is possible.

- **No file result reads another file.** `parser/file-state` and `parser/*file-memo*` are new for each file. Groups do
  no file I/O during the walk. Thus a cache for each file is safe for all groups.
- **Other inputs change the result.** The file result also depends on:
  - the file path, because each item holds `:file`, and the tests group checks the path against `:tests :paths`
  - the file extension, which selects the groups
  - the set of enabled groups (`--only`, `--disable`), because the groups share handlers
  - `--experimental-clj`, which changes the extensions
  - `.cljs-patrol/config.edn` and `--css-order`
  - the working directory, because the config path is relative
  - the platform charset, because the parser decodes the file with it
  - the cljs-patrol version

  `--fail-on` does not change the result. Time and the environment variables do not change the result.

- **The cached data is large.** EDN of all file results is approximately 4x the size of the source code. 70% of the size
  is two kinds of usages (style calls and test ID mentions), and approximately one third is the `:file` value in each
  item. Gzip makes it approximately 14x smaller.
- **A read of the cache is the main cost of a warm run.** An EDN read of all results takes approximately 2 seconds in a
  native binary. A SHA-256 hash of all files takes less than 0.5 seconds.

## Decision

Add an opt-in cache that keeps the full result of each file. Use the file content, not the file time, to find changed
files.

The design:

- **CLI.** `--cache-dir DIR` turns the cache on and sets its directory. Without this option, cljs-patrol uses no cache.
- **Layout.** One cache file for each source file: `<cache-dir>/v1/<run-key>/<entry-key>.edn`.
  - `<run-key>` is a hash of the inputs that are the same for all files in one `analyze-project` call: the cache schema
    version, the cljs-patrol version, the effective config, the sorted list of enabled groups, `--experimental-clj`, the
    platform charset and the source directory. Each source directory of a run thus has its own run key.
  - The config part of the run key is a canonical form of the effective config, with the keys in sorted order.
  - `<entry-key>` is a SHA-256 hash of the path length, the path and the SHA-256 hash of the content. The length makes
    the border between the path and the content clear, so two different files cannot give the same input to the hash.
- **Format.** EDN, the same format as the baseline. Each entry is
  `{:path <path> :content-hash <hash> :result <file result>}`. The writer removes `:file` from each item, and the reader
  adds it again. The writer binds `*print-length*`, `*print-level*` and `*print-namespace-maps*`, so that the printed
  form is complete.
- **Read.** The worker reads the file bytes one time. It hashes these bytes and, after a miss, parses the same bytes.
  Thus a file that changes during the run cannot make an entry that does not match its key. If the entry exists, and its
  `:path` and `:content-hash` are equal to the current values, cljs-patrol uses it (a hit). If not, it analyzes the file
  (a miss). The worker threads of `--jobs` do the reads and the analysis.
- **Write.** After a miss, the worker writes the entry to a temporary file in the same directory, made with
  `Files/createTempFile`. Then it moves the file into place with `fs/move-replace!`. The temporary file and the entry
  are on the same filesystem, so the move is atomic. Thus a reader never sees a partial entry, and the workers need no
  lock.
- **Failure.** All read errors are misses: a missing file, a corrupt file, a wrong path, a wrong content hash or a wrong
  schema. A write error gives a warning on stderr and does not stop the run. A cache problem never changes the findings.
- **Files with stderr output.** If a file writes to stderr during analysis, cljs-patrol does not cache its result. Today
  only a parse failure does this. The run must print the warning again on the next run.
- **Prune.** At the end of the analysis, before the report, cljs-patrol deletes:
  - the entries in the run keys of this run that no run used for 7 days
  - the other run-key directories that no run used for 7 days
  - the temporary files that are older than 1 day

  The prune runs before the report because some report paths call `System/exit`. A read of an entry updates its file
  time, so the file time shows the last use.

- **Dev builds.** A build without a release version (`"dev"`) does not use the cache, because its version cannot show a
  code change. cljs-patrol writes a warning to stderr when it ignores `--cache-dir`.
- **Timings.** `--timings` shows a `cache` phase (hash, read and write) and a line with the number of hits, misses and
  corrupt entries. Parse, walk and the slowest files show only the misses.

The design does not need extra rules for these cases, because each one only gives misses:

- symlinks, case-insensitive filesystems and different absolute workspace paths, because they change the path string
- a CI cache from another branch, because the key includes the content
- the system clock, because the cache does not use file times to find changes

[Options](#options) compares this choice with the other options.

## Options

### Opt-in `--cache-dir DIR` (chosen)

- Good, because CI must name a directory for `actions/cache`, and the flag names it.
- Good, because an opt-in flag writes no files into a project without a request.
- Good, because ESLint, Prettier and Stylelint also use an opt-in `--cache`.

### A cache that is on by default (rejected)

For example in `.cljs-patrol/cache`, like clj-kondo, Ruff, RuboCop and mypy.

- Good, because all runs get the speed-up with no setup.
- Bad, because a stale or wrong cache gives wrong findings without a sign, and cljs-patrol must never do that.

The opt-in flag can change to a default after the cache works well for some time.

### Pass markers only (rejected)

Like Ruff, Stylelint and Prettier, which store only "this file had no findings".

Rejected because it is not possible: the cross-file `analyze` step needs the declarations and usages of each file.

### File time and size to find changes (rejected)

The ESLint `metadata` strategy, which is the ESLint default.

Rejected because a fresh CI checkout sets new times on all files. Then all files are misses.

### One cache file for all results (rejected)

Like ESLint and Prettier.

Rejected because each run writes the full file again, one corrupt write loses all entries, the workers must take turns
to write, and the read was slower in the study.

### Shards that hold many entries (rejected)

Rejected because a change to one file writes a full shard again. With a codebase of this size, fewer cache files give no
gain.

### Delete all other run keys at each run (rejected)

Rejected because two configurations that share one cache directory, for example the CI groups and all groups, delete the
entries of each other at each run. It also deletes the run keys of the other source directories of the same run.

### Transit instead of EDN (rejected)

- Good, because its read was approximately 15% faster in a native binary.
- Bad, because it adds dependencies to the native binary.
- Bad, because it has the same keyword problem.

### A fact store as the cache (deferred)

Keep the file results as facts in an embedded Datalog database, and let a changed file remove its facts and add new
ones. The candidates are Clojure libraries that speak EDN. Datalevin stores the facts on disk in LMDB, and a warm run
queries them in place without a read of all entries. Datascript keeps the facts in memory, so it needs a write and a
read of the whole store, like the EDN cache. Asami is a graph store with Datalog-like queries, in memory or on disk.

- Good, because with an on-disk store a warm run does not read the whole cache, and the read of the cache is the main
  cost of a warm run.
- Good, because a rule that needs references across files, such as the schema roots in
  [ADR 0003](0003-tell-data-vectors-from-hiccup.md), can query them instead of building its own index.
- Good, because a developer can query the facts to explore a codebase, for example to find each vector `def` with a
  `:label` head.
- Bad, because an on-disk store adds native dependencies to the native binary, for example LMDB for Datalevin.
- Bad, because an on-disk store is a binary file for one platform, where an EDN file works on all platforms.
- Bad, because the cross-file `analyze` step of each group reads joined data today. To use the queries, the groups must
  change.
- Bad, because the run key and the entry key stay necessary, so it does not remove the risk of a stale cache.

It is deferred until a rule needs references across files. Then a prototype can compare it with the EDN cache.

### A full syntax graph in a graph database (rejected)

Load the full syntax tree of all files, with all attributes, into a property graph database, and let the rules query the
graph. Neo4j runs embedded only inside a JVM application, as a large library. Kùzu embeds like SQLite, but it is a C++
library that the native binary must bundle, and its repository was archived in 2025.

- Good, because one store holds everything that a rule or a search can ask.
- Bad, because the full tree is many times larger than the file results, which are already approximately 4x the source.
- Bad, because the rules walk rewrite-clj zippers. All groups must change to query the graph.
- Bad, because the graph databases that embed are JVM-only or native libraries, and none speaks EDN. The Datalog stores
  above fit the facts of a Clojure codebase better.

### A cache for each file and each group (rejected)

Rejected because it is not possible: the groups share handlers in one walk.

## Consequences

- Estimated warm-cache run time in a native binary on 1 core, from the study:

  | Changed files | CI groups, compared with no cache | All groups, compared with no cache |
  | ------------- | --------------------------------- | ---------------------------------- |
  | 0             | approximately 5.5x faster         | approximately 9x faster            |
  | 10            | approximately 5.4x faster         | approximately 8.6x faster          |
  | 100           | approximately 4.4x faster         | approximately 6.4x faster          |

  These numbers keep `:file` in each item. Without `:file`, the CI groups with 0 changed files are approximately 7x
  faster. All numbers are estimates. The study measured the EDN read in Babashka, not in the cljs-patrol binary.

- A cold run with the cache on is slower by approximately 1 to 1.5 seconds, for the EDN write.
- The read of the cache becomes the largest part of a warm run. A smaller result (for example, fewer style-call usages)
  makes warm runs faster.
- The output, the exit code, `--baseline` and `--baseline-write` stay the same, because the joined data is the same.
- A local build from changed source with `-Dcljs-patrol.version=X` uses the cache of release X. Such a build must use a
  different cache directory.
- A CI setup needs an `actions/cache` step with a key such as `<version>-<config hash>-<commit>` and the restore key
  `<version>-<config hash>-`. Then a run gets the latest older cache.

## Implementation

Do these steps in separate pull requests:

1. Fix the keywords and symbols that do not survive an EDN write and read. Either the parser makes a different value for
   aliases from string requires, or the cache uses a reader tag. Check first if a rule depends on the current form. Add
   a test that each value of each file result is equal to its EDN write and read.
2. Add the cache module: the run key, the entry key, the EDN read and write with the `:file` removal, the atomic write
   and the failure rules. Add unit tests for the round trip and for corrupt entries.
3. Connect the cache to `analyze-files`. A worker reads the file bytes one time, checks the cache, and parses the same
   bytes after a miss. It writes the entry after a miss. Add `--cache-dir`, the prune step, the `--timings` lines, and
   the cold and warm integration tests.
4. Measure a cold run and a warm run on a large codebase with the native binary. Update this ADR with the results.
5. Add the `actions/cache` step to the CI of a project that uses cljs-patrol.

## Confirmation

Before the status changes to `Accepted`, the measurement in Implementation step 4 must show the warm-run gain, and the
tests must show these facts:

- A cold run and a warm run give the same stdout, stderr and exit code.
- Each value in each file result is equal to its EDN write and read. This includes aliases from string requires with `@`
  or `/`.
- No metadata in a file result changes the findings, because EDN does not keep metadata.
- A changed file is analyzed again.
- A file that changes during the run does not make a wrong entry.
- A change of config, flags or version gives misses.
- A truncated or invalid entry gives a fresh analysis.
- Two runs with different config in one cache directory both get hits.
- The output with `--jobs 1` and `--jobs 4` is the same.

## Revisit when

- The warm-run measurement in step 4 shows less than 3x gain on 1 core. Then make the file result smaller first, or find
  a faster format.
- The file result starts to depend on other files. Then the run key or the entry key must include those files, or the
  cache must stop for that group.
- cljs-patrol adds a new input that changes the file result, for example a new config file or a new flag. Then the run
  key must include it.
- A rule needs references across files, such as the schema roots in [ADR 0003](0003-tell-data-vectors-from-hiccup.md).
  Then prototype the fact store, and compare its warm run and its native binary with the EDN cache.
- A rule reads a value that is not in the run key or the entry key, for example a file outside the source directory or
  an environment variable. In other tools, almost all known stale-cache bugs came from an input that was not in the key.

## More Information

- Parallel analysis: merge commit `cd79eeb551`, pull request https://github.com/olecve/cljs-patrol/pull/138
- ESLint cache: https://github.com/eslint/eslint/blob/v10.14.0/lib/cli-engine/lint-result-cache.js
- Prettier cache: https://github.com/prettier/prettier/blob/3.10.1/src/cli/format-results-cache.js
- Stylelint cache: https://github.com/stylelint/stylelint/blob/18.0.0/lib/utils/FileCache.mjs
- Ruff cache: https://github.com/astral-sh/ruff/blob/0.17.4/crates/ruff/src/cache.rs
- RuboCop cache: https://github.com/rubocop/rubocop/blob/v1.92.2/lib/rubocop/result_cache.rb
- mypy incremental cache: https://github.com/python/mypy/blob/v2.4.1/mypy/build.py
- clj-kondo cache: https://github.com/clj-kondo/clj-kondo/blob/v2026.10.26/src/clj_kondo/impl/cache.clj
- golangci-lint cache: https://github.com/golangci/golangci-lint/blob/v2.15.0/internal/cache/cache.go
- Stale-cache bugs from inputs that were not in the key: Prettier plugins
  (https://github.com/prettier/prettier/issues/17260), Ruff nested config
  (https://github.com/astral-sh/ruff/issues/12721), RuboCop external state
  (https://github.com/rubocop/rubocop/pull/7496)
- GraalVM native-image security services:
  https://www.graalvm.org/latest/reference-manual/native-image/dynamic-features/JCASecurityServices/
- Datalevin: https://github.com/datalevin/datalevin/blob/1.2.10/README.md and its native build notes,
  https://github.com/datalevin/datalevin/blob/1.2.10/doc/install.md
- Datascript: https://github.com/tonsky/datascript/tree/1.8.1
- Asami: https://github.com/quoll/asami/tree/v2.3.4
- Neo4j embedded: https://neo4j.com/docs/java-reference/current/java-embedded/
- Kùzu, archived: https://github.com/kuzudb/kuzu
