# ADR 0013: Read And Write Files Through NIO Paths, And Test With An In-Memory File System

## Status

Accepted, 2026-11-24 (proposed 2026-11-23)

## Context

`src/cljs_patrol/fs.clj` wraps `java.io.File`. Each public function takes path strings and makes a `(File. path)`.
`baseline` and `reporters.html` also read and write files with `slurp`, `spit` and `io/writer` on path strings.

`java.io.File` has three properties that hide errors:

- **A nil parent is the working directory.** `(File. nil "src")` gives the path `src`, and the JVM resolves it against
  the working directory. On 2026-11-21, a mutant from the mutation runner of
  [ADR 0005](0005-mutation-testing-in-one-jvm.md) made the test path function `fs/tmp-file-path` return nil. Then
  `fs/join-path` gave paths without a root, and the test helper `scratch-tree` of `fs_test` wrote its 7 fixture files
  into the repository root. The fix is now on `main`: `fs/tmp-dir` and `fs/tmp-file-path` are deleted, the tests take
  their temporary folders from the test helper `cljs-patrol.temp-dir` (`Files/createTempDirectory`), and the file guard
  of the runner stops a run when a test changes a file.
  [ADR 0011](0011-mutation-test-stop-when-a-test-changes-a-file.md) records that fix, and
  [ADR 0012](0012-mutation-test-guard-skips-what-git-ignores.md) sets the paths that the guard skips. `fs/join-path`
  still gives a path without a root for a nil `base`. Step 1 of this ADR fixes that: `fs/join-path` then throws on a nil
  `base`. This ADR does not depend on ADR 0011 or ADR 0012.
- **Failures are booleans.** `File.delete` and `File.mkdirs` return `false` and give no reason. `fs/delete-file!` and
  `fs/mkdirs` return that value, and `fs/delete-tree!` drops it. No caller of the three reads it.
- **Failures are empty lists.** `File.listFiles` returns nil when it cannot read a directory. The private function
  `walk-files` of `fs` maps over that nil. Discovery thus finds no files in an unreadable directory and prints nothing.

`java.nio.file.Path` and `java.nio.file.Files` do not have these properties. A probe on Clojure 1.12.7 showed these
facts:

- `Path/of` with a nil first part, and `Path.resolve` with a nil argument, throw `NullPointerException`.
- `Files/delete` on a missing file throws `NoSuchFileException` with the path. `Files/createDirectories` under a regular
  file throws `FileSystemException` with the reason "Not a directory". For the same call, `File.mkdirs` returns `false`.
- A `Path` belongs to one `FileSystem`. Code that gets its paths from a `FileSystem` argument works on any file system,
  also on an in-memory one. A `Path` from another file system gives `ProviderMismatchException`, not a wrong result.

The unit tests of file code use the real disk today. Each test makes a temporary directory with `cljs-patrol.temp-dir`
and deletes it. A test can still leave files behind, and the file guard of the runner finds them only in a mutation run.
An in-memory file system removes these problems for the tests that do not need the real disk.

The scope is small. In `src/`, 20 lines call `fs/`: 10 in `baseline`, 3 in `core`, 1 in `parser` and 6 in the reporters.
2 public functions of `fs` (`absolute-path?` and `delete-tree!`) have no caller in `src/` and serve only tests. In
`test/`, 27 lines in 6 namespaces call `fs/`.

### Where paths come in and go out

- **CLI arguments.** The source directories and `--files` are strings. `core` checks the directories with
  `fs/missing-dirs` and prints the missing ones as typed (`core.clj:422-424`).
- **Config.** `baseline/read-config` reads `.cljs-patrol/config.edn` in the working directory (`baseline.clj:362-378`).
  The config can set the baseline path.
- **Discovery.** `fs/list-source-files` returns strings. `parser/analyze-file` makes `file-path` with `(str file)`
  (`parser.clj:534`). Each finding, each timing record and each parse warning holds that string as `:file`.
- **The baseline file.** `resolve-baseline-path` joins the first source directory and `.cljs-patrol/baseline.edn`
  (`baseline.clj:213`). The baseline key `:file` is `(str (.relativize base target))` on absolute paths
  (`fs/relativize`), so it uses the separator of the OS.
- **The reporters.** The console prints `:file` as it is (`console.clj:22`). EDN, Markdown and the HTML `vscode://` link
  print `fs/absolute-path` of it. The HTML report goes to `report.html` in the working directory (`core.clj:341, 361`).
- **The tests group.** `under-path?` splits the `:file` string on `/` and `\` (`groups/tests.clj:140-155`).
- **The cache of [ADR 0002](0002-per-file-result-cache.md).** It is not on `main` yet. Its entries are EDN and hold
  `:path` strings. Its design writes with `Files/createTempFile` and `fs/move-replace!`.
- **Resources.** `group`, `groups.css-order.orders` and `reporters.html` `slurp` a classpath resource. This ADR does not
  change them.
- **The native image.** `build.clj` compiles `src/` with `*warn-on-reflection*`, and `reflection_test` fails on a
  reflective call. The `native-verify` job in `.github/workflows/test.yml` runs the binary from `test-projects/a11y-app`
  with `--baseline-write src` and `--baseline src`. The basis in `build.clj` uses no alias, so a dependency of the
  `:test` alias is not in the uberjar or the native image.

## Decision

`fs` and its callers in `src/` read and write files through `java.nio.file.Path` values from a `FileSystem` that the
caller gives. Unit tests of file code run on Jimfs, an in-memory file system, which only the `:test` alias loads.

The parts of the decision:

- **The `FileSystem` comes from `-main`.** `core/-main` gets `(FileSystems/getDefault)` at run time. It makes a `Path`
  from each CLI argument and from each fixed relative path (`.cljs-patrol/config.edn`, `report.html`). No top-level
  `def` holds a `Path` or a `FileSystem`. `graal-build-time` initializes Clojure classes when the native image is built,
  so such a `def` would keep a value from the build machine.
- **`fs` uses NIO directly.** `fs.clj` calls `Path` and `Files` with type hints. It uses no library for this. See
  [babashka/fs](#babashkafs-as-the-base-of-the-nio-layer-rejected).
- **Strings stay where text leaves the program or enters data.** These keep their text:
  - `:file` in each finding, timing record and parse warning. A `Path` does not print as readable EDN, and the cache of
    ADR 0002 writes file results as EDN.
  - The baseline key `:file`. It stays `(str (.relativize base target))`. This ADR does not change the separator.
  - The paths that the reporters and the error messages print.
- **`Path` values flow in between.** A source directory, each discovered file until `analyze-file` makes `file-path`,
  the baseline path, the config path and the report path are `Path` values.
- **The parser reads bytes.** `z/of-file` cannot take a `Path`. rewrite-clj 1.1.47 opens the file with `io/file`
  (`reader.cljc:254-262`), and `io/file` has no coercion for `Path` in Clojure 1.12.7 (a probe gives
  `IllegalArgumentException`). The parser reads the bytes with `Files/readAllBytes`, decodes them as UTF-8 with
  `(String. bytes UTF_8)`, and calls `z/of-string`. `z/of-string` normalizes newlines like `z/of-file`. The parser does
  not use `Files/readString`. That function throws `MalformedInputException` on bytes that are not UTF-8, and the reader
  of `z/of-file` puts U+FFFD in their place (a probe showed both).
- **Discovery keeps its behavior.** It walks with `Files/newDirectoryStream` and `Files/isDirectory`. These follow a
  symbolic link to a directory, like `File.isDirectory` and `File.listFiles`. Discovery does not use
  `Files/walkFileTree`, because it does not follow links by default.
- **An unreadable source directory gives a warning.** Discovery skips that directory and goes on, as today. It prints
  one warning on stderr that names the directory and the reason, such as `AccessDeniedException`. It does not stop the
  run, because one unreadable directory must not block a whole lint run. Today the skip is silent.
- **Tests that need the real disk stay on it.** These are the `dev/` tests that run `git`, the end-to-end CLI tests, the
  tests that read `test-projects/`, and the `native-verify` job.
- **Out of scope.** `dev/` and `build.clj` keep `java.io`. `dev/` needs `git` and the real disk, and `tools.build` takes
  strings.

## Options

### Keep `java.io.File` (rejected)

- Good, because it needs no change, and all callers and tests work today.
- Bad, because a nil parent resolves against the working directory with no error. This made a test write into the
  repository root.
- Bad, because `delete` and `mkdirs` return `false` with no reason, and `listFiles` returns nil for an unreadable
  directory.
- Bad, because `File` works only on the default file system, so the file tests need the real disk.

### NIO with Jimfs for the tests (chosen)

- Good, because nil throws, and failures throw with a reason.
- Good, because Jimfs implements `java.nio.file`, so the code under test is the production code.
- Good, because Jimfs can model Windows paths (`Configuration/windows`). A probe on Jimfs 1.3.2 relativized
  `C:\work\src\x\a.cljs` against `C:\work` to `src\x\a.cljs`. A test can thus pin the baseline key text for Windows. CI
  has no Windows runner.
- Good, because `Files/move` with `ATOMIC_MOVE` replaces an existing file on Jimfs 1.3.2, as on the default file system
  (a probe). The baseline write, a temporary file and then a move, thus works the same in a test.
- Good, because Google maintains it. Its license is Apache-2.0, and 1.3.2 is from 2026-08-18.
- Bad, because Jimfs brings Guava 33.7.1-jre and 5 small jars into the test classpath.
- Bad, because Jimfs models Unix behavior, and its README says that it "may not exactly match any particular real file
  system". Tests on the real disk stay necessary.

### memoryfilesystem by Philippe Marschall (deferred)

- Good, because it has no dependencies, and its license is MIT (README FAQ).
- Good, because it supports `ATOMIC_MOVE`, detects symbolic link loops, and checks POSIX and DOS access.
- Bad, because its README answers "Does it have bugs?" with "Quite likely."
- Bad, because it models users, groups, umask and access checks, which cljs-patrol does not need.

Both libraries implement `java.nio.file`, so the production code does not change if the tests change library.

### Apache Commons VFS with `ram://` (rejected)

Rejected because its API is `FileObject`, not `java.nio.file.Path`. Then `fs` needs a second code path for tests, and
the tests do not run the production code.

### babashka/fs as the base of the NIO layer (rejected)

- Good, because it is maintained, its license is EPL-1.0 like Clojure, and it sets `*warn-on-reflection*`.
- Good, because a `Path` argument goes to `Files` unchanged, so many of its functions work on Jimfs.
- Bad, because a string argument always goes to the default file system: `as-path` calls `(.toPath (io/file path))`
  (`fs.cljc:52-57`). A probe showed `(fs/exists? "/work/src/a.cljs")` is `false` for a file that exists only on Jimfs. A
  string that a caller forgets to make a `Path` thus leaves the given file system with no error.
- Bad, because `(fs/path nil "src")` returns `src` on the default file system (`fs.cljc:88-98`, and a probe). This is
  the nil hazard of `(File. nil "src")` again.
- Bad, because `file-name`, `file`, `slurp`, `spit`, `canonicalize` and `delete-on-exit` call `Path.toFile`
  (`fs.cljc:60-64`). For a Jimfs `Path`, that throws `UnsupportedOperationException` (a probe of `file-name`).
- Bad, because `glob` and `match` get their `PathMatcher` from `FileSystems/getDefault` (`fs.cljc:661`).
- Bad, because it adds a runtime dependency to the uberjar and the native image, for the few `Path` and `Files` calls
  that `fs` needs.

## Consequences

- A nil path throws `NullPointerException` in production and in tests. A test like `fs_test` with that mutant fails and
  writes nothing.
- In the `Path` functions of `fs`, a failed delete and a failed directory create throw with the reason. Today they are
  ignored.
- A run on a tree with one unreadable directory skips that directory, as today, but now prints a warning with the
  directory and the reason. The run goes on, and its exit code does not change. A person who does not read stderr can
  still miss the files of that directory.
- The unit tests of `fs`, of the baseline write and read, of the config read and of the HTML write need no temporary
  directory and no clean-up.
- A test can check Windows paths on Jimfs.
- All paths of one run must come from one `FileSystem`. A mix gives `ProviderMismatchException`. The cache of ADR 0002
  must take the `FileSystem` when it is built, and make its temporary file in the directory of its target `Path`.
- Each `Files` call with varargs needs a typed array, such as `(make-array LinkOption 0)`. Without an array, the call
  does not compile. An array of the wrong type, such as `(into-array [])`, compiles and throws `ClassCastException` at
  run time (a probe of `Files/exists`).
- The `:test` alias gets Jimfs and Guava. The uberjar and the native image do not change.
- `dev/` and `build.clj` still use `java.io`. The codebase thus has two styles until `dev/` moves.

## Confirmation

The migration is done when these checks pass. Step 1 passes the checks of the uberjar, the native image and the same
output as `main`. Steps 2 to 4 make the others pass.

- `clojure -M:test` passes, and each test of `fs_test` that needs no real disk runs on Jimfs.
- A test shows that each public function of `fs` throws on a nil path.
- `grep -rnE 'java\.io\.File|io/file|\(File\. |io/writer|io/reader|\bspit\b' src/` finds nothing, and each `slurp` in
  `src/` reads an `io/resource`.
- `clojure -T:build uber` passes, and `unzip -l target/cljs-patrol-dev.jar | grep -ci jimfs` prints `0`.
- The `native-verify` job passes.
- On `test-projects/a11y-app`, `--baseline-write src` on `main` and on the branch give files that differ only in
  `:generated-at`.
- On each `test-projects/*-app`, the console, EDN and Markdown output of `main` and of the branch are equal.
- A Jimfs test with `Configuration/windows` pins the text of a baseline key with `\`.

## Implementation

Each step is one pull request, and each one ships on its own.

1. **Jimfs, and `fs` takes and returns `Path`.** Add `com.google.jimfs/jimfs {:mvn/version "1.3.2"}` to `:extra-deps` of
   `:test`. Add `Path` functions that take a `FileSystem` or a `Path`, and test them on Jimfs. Write them with `Files`:
   `createDirectories`, `deleteIfExists`, `move`, `newDirectoryStream`, and `walkFileTree` for `delete-recursively!`.
   The `Path` functions give the results of NIO, with one added rule: they refuse nil and the empty path. Keep each
   string function as an adapter that gives the same result as `java.io` gave. An adapter that `java.io.File` matches
   most simply uses it, because step 4 removes the adapters.
   - The `Path` functions refuse nil and the empty path. NIO reads the empty path as the working directory. Each public
     `Path` function throws `NullPointerException` on a nil `Path` argument, and `IllegalArgumentException` on an empty
     `Path` argument. `path` also throws `IllegalArgumentException` when its parts make the empty path.
   - `resolve-path` gives the result of `Path.resolve`: an absolute leaf replaces the base.
   - `relative-path` gives the result of `Path.relativize` on the two absolute paths. A target outside the base gives a
     path up from the base, such as `../other/a.cljs`. A target that is the base throws `IllegalArgumentException`,
     because the result is the empty path. A target with another root than the base, such as another drive on Windows,
     throws `IllegalArgumentException` too. Only the adapter `relativize` falls back to the text of the target.
   - The adapters keep the results of `java.io` for the empty string. `file-exists?`, `absolute-path?`, `mkdirs` and
     `delete-file!` give `false`, `parent-dir` gives nil, `delete-tree!` does nothing, `list-source-files` gives `[]`,
     `missing-dirs` reports it, and `move-replace!` throws `NoSuchFileException`. `absolute-path` gives the working
     directory, `relativize` reads an empty base as the working directory, and `(join-path "" "x")` gives `/x`. `mkdirs`
     and `delete-file!` check for the empty string themselves, because from JDK 24, `java.io.File` reads the empty path
     as the working directory too.
   - `(join-path "src" "/abs")` gives `src/abs`, as in `java.io`. `join-path` throws on a nil `base`.
   - NIO throws `InvalidPathException` on a string that `java.io` accepts, such as one with a NUL character. In the
     adapters, such a string names no file. `file-exists?` gives `false`, a create or a delete does nothing and gives
     `false` or nil, a listing gives `[]`, `missing-dirs` reports it, and `move-replace!` throws `NoSuchFileException`.
     The adapters that give a path text, and `absolute-path?`, give the result of `java.io`: `absolute-path`,
     `parent-dir` and `join-path`. `relativize` gives back the text of the target.
   - `mkdirs` gives `true` when it made the folder, and `false` when it did not, as in `java.io`. `delete-file!` gives
     `false` and does not throw when it cannot delete, as in `java.io`. It throws only on nil.
   - `delete-tree!` does not follow a link to a folder. It deletes the link, and the folder behind it stays. `java.io`
     deleted the files behind the link. This change is intended, because a delete must stay in its tree.
   - The warning for an unreadable directory names the class of the exception, and its reason when the reason is not
     blank.
   - Risk: the text of the discovered paths changes. `File` and `Path` both drop a trailing `/` and a doubled `/`, and
     both keep `./` (a probe gave `./src/a` for `./src//a/` with both). A test compares the list for `src`, `src/` and
     `./src` with the list of `main`, in order.
   - Risk: a symbolic link to a directory is not followed. A test with a link on Jimfs proves that it is.
   - Risk: an unreadable directory stops the run. Discovery catches the `IOException` or the
     `DirectoryIteratorException` of that directory, prints one warning on stderr with the directory and the reason, and
     goes on, because one unreadable directory must not block a whole lint run. A test on the real disk proves that the
     run finds the other files and that the warning names the directory.
   - Risk: reflection. `reflection_test` proves there is none.
   - Risk: Jimfs gets into the uberjar. The `unzip -l` check proves it does not.
   - Risk: Guava conflicts with a test dependency. `clojure -M:test` proves it does not.
2. **The callers use `Path`.** `core/-main` makes the `Path` values. `baseline` writes with `Files/newBufferedWriter`
   and reads with `Files/readAllBytes`. `reporters.html` writes with `Files`. `parser` reads bytes as the Decision says.
   `read-config` takes the config `Path`. Each caller moves to the `Path` functions on purpose, because their results
   are the results of NIO and not of `java.io`. For each of these rules, a caller keeps the rule of `java.io` in its own
   code, or drops it on purpose:
   - An empty base: `java.io` joins a leaf under the root, and NIO reads the empty path as the working directory. `path`
     throws on it.
   - An absolute leaf: `java.io` puts it under the base, and `resolve-path` gives the leaf.
   - A target outside the base: `relativize` gives the text of the target, and `relative-path` gives a path up from the
     base. A target that is the base: `relativize` gives the empty string, and `relative-path` throws.
   - `mkdirs` on an existing file: `java.io` gives `false`, and `create-dirs!` throws.
   - A failed delete in the `finally` of `write-baseline`: `delete-file!` gives `false`, and `delete-if-exists!` throws.
     A throw there hides the first error.

   The baseline byte compare on `test-projects/a11y-app`, and the compare of the console, EDN and Markdown output on all
   17 `test-projects/*-app` against `main`, prove the result.
   - Risk: the baseline keys change. The integration baseline tests and the byte compare on `test-projects/a11y-app`
     prove they do not.
   - Risk: the printed paths change. The reporter tests and the output compare on `test-projects/` prove they do not.
   - Risk: a file that is not UTF-8 parses differently. A test with the bytes `C3 28` proves the parser gives the same
     result as `main`.
   - Risk: the native image. The `native-verify` job proves the binary writes and compares a baseline.

3. **The tests move to Jimfs.** Move the tests of `fs_test` that need no real disk, the baseline write, read and config
   tests of `baseline_test`, the write tests of `reporters/html_test`, the baseline write tests of `core_test` and the
   discovery tests of `parser_test` to Jimfs. These tests then do not use `cljs-patrol.temp-dir`. The tests on the real
   disk keep it. In `fs_test`, these are the tests of an unreadable directory, the compare of the discovered paths with
   `java.io`, and the tests of the string adapters. Add the Windows test of the baseline key.
4. **Remove the string adapters.** Remove each string function of `fs` that has no caller. Keep `nano-time` while the
   baseline write names its temporary file with it.
   - Risk: a caller still passes a string. A string where a `Path` is hinted gives a `ClassCastException`, and
     `clojure -M:test` finds it. The `grep` in Confirmation proves no `java.io` file access is left in `src/`.

## Revisit when

- A Clojure release extends `clojure.java.io/Coercions` to `Path`. Then rewrite-clj's `z/of-file` accepts a `Path`, and
  the parser can use it again.
- A babashka/fs release makes `as-path` take a `FileSystem`, or stops making paths from strings with `io/file`. Its
  CHANGELOG shows this. Then compare it with the hand-written `fs` again.
- Jimfs has no release by 2028-08-18, 2 years after 1.3.2, or a JDK that CI uses breaks it. Then move the tests to
  memoryfilesystem.
- A test needs file permissions, owners or case-insensitive names that Jimfs does not model. Then try memoryfilesystem
  for that test.
- CI gets a Windows runner, or an issue reports baseline keys with `\`. Then decide the separator of baseline keys in a
  new ADR.
- An issue reports findings that a run missed in an unreadable directory, or asks that such a directory stop the run.
  Then decide if an unreadable directory stops the run.
- The mutation runner moves to a project of its own. Then decide `java.io` in `dev/` there.

## More Information

- The code: [`src/cljs_patrol/fs.clj`](../../src/cljs_patrol/fs.clj),
  [`test/cljs_patrol/fs_test.clj`](../../test/cljs_patrol/fs_test.clj), [`build.clj`](../../build.clj)
- Related: [ADR 0001](0001-native-image-garbage-collector.md) (the native image),
  [ADR 0002](0002-per-file-result-cache.md) (the cache),
  [ADR 0011](0011-mutation-test-stop-when-a-test-changes-a-file.md) (temporary folders in tests, and the file guard),
  [ADR 0012](0012-mutation-test-guard-skips-what-git-ignores.md) (the paths that the file guard skips)
- Jimfs. [README](https://github.com/google/jimfs/blob/v1.3.2/README.md), at v1.3.2. The README at that tag still names
  1.3.0 as the latest release. Maven Central lists 1.3.2:
  https://central.sonatype.com/artifact/com.google.jimfs/jimfs/1.3.2
- memoryfilesystem. [README](https://github.com/marschall/memoryfilesystem/blob/2.8.4/README.md), at 2.8.4.
- babashka/fs. [README](https://github.com/babashka/fs/blob/v0.5.34/README.md) and
  [source](https://github.com/babashka/fs/blob/v0.5.34/src/babashka/fs.cljc), at v0.5.34
- Apache Commons VFS.
  [RAM provider](https://github.com/apache/commons-vfs/tree/rel/commons-vfs-2.10.0/commons-vfs2/src/main/java/org/apache/commons/vfs2/provider/ram),
  at 2.10.0
- rewrite-clj.
  [`file-reader` and `string-reader`](https://github.com/clj-commons/rewrite-clj/blob/v1.1.47/src/rewrite_clj/reader.cljc#L254-L270),
  at v1.1.47
- Java 21.
  [`Path.toFile`](<https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/file/Path.html#toFile()>)
  throws `UnsupportedOperationException` for a `Path` that is not on the default provider.
  [`Files.readString`](<https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/file/Files.html#readString(java.nio.file.Path)>)
  throws `MalformedInputException` on bytes that are not valid.
- Step 1, the pull request: [olecve/cljs-patrol#179](https://github.com/olecve/cljs-patrol/pull/179)
