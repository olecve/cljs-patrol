# ADR 0011: Stop A Mutation Run When A Test Changes A File

## Status

Accepted, 2026-11-23 (proposed 2026-11-23); partially superseded by ADR 0012

## Context

This repository has a dev-only mutation runner ([ADR 0005](0005-mutation-testing-in-one-jvm.md)). A mutation runner
checks the tests. It plants one small change, a mutant, in the code under test, and runs the tests. When a test fails,
the mutant is killed. When all tests pass, the mutant survived, and no test checks that code.

- **One JVM.** This runner runs every mutant in one JVM, in the directory where the command starts. It loads the changed
  form into the running JVM, runs the tests, and then loads the original form again. This last step is the restore. The
  restore does not put back a file that the tests changed.
- **The baseline.** Before the first mutant, the runner runs the tests once on the unchanged code. In this baseline run,
  it records which tests call each function: the record of test calls. Each mutant then runs only the tests that call
  its function.
- **The timeout.** The tests of a mutant time out when they run longer than 1.5 times their own baseline run, plus 2 s.
  A test thread that still runs after the timeout disturbs every later mutant, so the run stops there.
- **The option `-d`.** `-d NAME` runs only the mutants of the named top-level definition.
- **The mutant id.** The id names the function, the line and column, and the change, such as
  `tmp-file-path/55:3/body->nil`: the body of `tmp-file-path`, at line 55, column 3, becomes `nil`.
- **The cache.** The runner writes the outcome of each finished mutant to a cache file, `.mutate-cache/<target-ns>.edn`
  in the directory where the command starts ([ADR 0010](0010-mutation-test-cache-and-resume.md)). The next run reuses
  those outcomes, so a stopped run goes on from where it stopped.
- **The exit code.** The runner exits with 0 when the tests kill every mutant. It exits with 1 when a mutant survived or
  no test called its function. It exits with 2 when the run cannot start or stops early.

On 2026-11-21, a run of `clojure -M:test:mutate cljs-patrol.fs -t cljs-patrol.fs-test` left 7 files in the repository
root: `src/kept.clj`, `src/kept.cljs`, `src/skipped.txt`, `.hidden/secret.clj`, `node_modules/thing/vendored.cljs`,
`target/classes/app/stale.clj` and `out/compiled.cljs`. The function `fs/tmp-file-path` returns a new path in the OS
temporary folder. The mutant `tmp-file-path/55:3/body->nil` made it return `nil`. The test helper `scratch-tree` in
`fs_test.clj` took its root folder from that function, so the root was `nil`. `(java.io.File. nil "src")` is the
relative path `src`, so the helper wrote its 7 fixture files into the working directory. The cleanup,
`(fs/delete-tree! nil)`, deletes a folder and all its files. With `nil` it threw, so it deleted nothing. The test
failed, the mutant counted as killed, and the report showed no sign of the files.

A worse mutant can overwrite or delete a real file, also a file that is not committed yet. Four other test files took a
temporary path from `fs/tmp-file-path` or `fs/tmp-dir` in the same way: `baseline_test.clj`, `core_test.clj`,
`integration/baseline_test.clj` and `reporters/html_test.clj`.

A JVM cannot move its tests to another directory. Since JDK 11, `java.io.File` reads `user.dir` once, at startup, so a
change of the property does not change how a relative path resolves. `deps.edn` does not pin a JDK. The measurements
below use JDK 22.

## Decision

The runner gets a guard: after each mutant that loaded, it compares the files and folders of the working directory with
the reference, the last snapshot with no change, and stops the run when they differ. The tests take their temporary
folders from the JDK, not from the code under test.

- **Snapshot.** A snapshot holds the path of every file and folder under the directory the command runs from, and the
  size and the time of change of each file. A folder has no size or time in it, since its time changes when its files
  change, so a new or deleted folder counts, an empty one too. The snapshot does not enter the `.git/` folder of that
  directory, the cache folder of the run, which is `.mutate-cache/` by default, and the folders that tools write while a
  run goes on: `.lsp/`, `.clj-kondo/.cache/`, `.clj-kondo/imports/`, `.clj-kondo/inline-configs/`, `.cpcache/`,
  `.shadow-cljs/`, `.idea/` and `.calva/`. A folder that holds one of these, such as `.clj-kondo/`, is not in the
  snapshot itself, since a tool can make it, but its other files, such as `.clj-kondo/config.edn`, are. It also leaves
  out, at any depth, the names `*.swp`, `*.swo`, `4913`, `.#*`, `#*#`, `*~`, `.DS_Store` and `.nrepl-port`, which
  editors, macOS and nREPL write. One list in the guard holds these folders and names. Other folders that git ignores,
  such as `target/` and `node_modules/`, count. A file that is deleted while the snapshot is taken is left out, and a
  folder that cannot be read counts as one entry, "unreadable". When two snapshots are equal, the comparison ends at
  once.
- **Baseline.** The guard takes one snapshot before the baseline and one after it. The files that differ between the two
  are the files that the baseline wrote. The guard compares each later snapshot with the reference: the last snapshot
  with no change, which is the snapshot after the baseline at first.
- **Which mutants.** Every mutant that loaded gets a snapshot, also one that failed while its form loaded, since that
  form ran and can write a file. A mutant that cannot be read or compiled, an unviable mutant, gets no snapshot.
- **Files that the baseline wrote.** A file that the baseline added or changed counts as changed only when its bytes
  change, because its test writes it again for each mutant. When its bytes change, the message says that the baseline
  wrote it too. A file whose bytes cannot be read counts as changed.
- **Stop.** The run stops with the id of the mutant that just ran and up to 10 paths, and exits with 2. For a mutant
  that failed while it loaded, no test ran, so the message says that loading the mutated definition, or putting the
  original back, changed the files, and that the person looks at the top-level code of that definition. It says that it
  did not undo the files. When the tests of that mutant still run after the timeout, or its restore failed, the message
  says that too. The cache drops the outcome it held of that mutant, so the next run runs it again, and the report does
  not list that mutant as one the run did not reach. The guard reads the cache file too. When the file holds an outcome
  with the id of that mutant, or with its place, which `--iterate` matches, the guard writes the file again without that
  one outcome and keeps the rest as it was. This is also true in a run with `--fresh`, or one that starts over, which
  holds none of the file's outcomes. When the file holds no outcome of the mutant, the guard writes nothing.
- **Tests.** A new test helper, `cljs-patrol.temp-dir`, makes each temporary folder with the JDK call
  `Files/createTempDirectory`, a new one for each call. The folder is new, empty, and in the OS temporary folder. The
  tests delete the folders that they make with `temp-dir/delete-dir!`, `temp-dir/with-dir` or the fixture
  `temp-dir/delete-created`. The deletion does not follow a link. `fs_test.clj` and the four other test files above use
  it. `fs/tmp-dir` and `fs/tmp-file-path` are deleted, since nothing calls them now.

## Options

### A snapshot after every mutant that loaded (chosen)

- Good, because it finds a new, changed or deleted file or folder outside the skipped folders, the folders that git
  ignores too, however the file was made: a write, a rename, a move, a copy, an empty create or a truncate.
- Good, because a change that the snapshot finds appeared while the mutant that just ran was loaded, so the message
  names that mutant.
- Good, because the guard is small: about 160 lines.
- Good, because the cost stays under 2 times the run without it. The times are medians of 3 runs in the same JVM, with
  and without the guard, on JDK 22, in a worktree of this repository whose snapshot holds 514 files and folders, at a
  load average of 3.7 to 4.1. On `cljs-patrol.groups.reagent`, with `-d` for every definition but `element-children`,
  whose mutant at line 302 loops when the machine is busy, the 229 mutants took 229 snapshots in 3.02 s. The run went
  from 3.70 s to 6.76 s, 1.83 times as long. On `cljs-patrol.fs`, the 27 mutants took 27 snapshots in 0.31 s, and the
  run went from 0.39 s to 0.73 s, 1.87 times as long.
- Good, because it found the real case: with the old `fs_test.clj`, the run stopped at `tmp-file-path/55:3/body->nil`
  and named the 7 files. On today's tree, a scratch test does the same with `fs/parent`, as Confirmation describes.
- Bad, because it only reports. A deleted file that git does not hold is lost.
- Bad, because it does not see a file outside the working directory, such as one in the home folder.
- Bad, because another process that changes a file during the run, such as an editor that saves a source file, also
  stops the run, and the message names the mutant that just ran.
- Bad, because it does not see a change in the skipped folders and names, or a change that keeps the size and the time
  of change of a file.
- Bad, because a background thread of an earlier test that writes a file late makes the guard blame the mutant that runs
  then.
- Bad, because a macro runs while the runner expands it, also in an unviable mutant. The next snapshot finds a file that
  such a macro writes, and blames the mutant that it follows. When no snapshot follows, the file stays unseen.

### A snapshot only after mutants whose tests write, found with Java Flight Recorder (rejected)

Java Flight Recorder (JFR), the event recorder of the JDK, records a `jdk.FileWrite` event for each write call of the
JVM. In the baseline, a test that wrote a file marked its mutants for a snapshot, and a second recording during the
mutants told a write of this run from a write of another process.

- Good, because fewer mutants get a snapshot. On `cljs-patrol.groups.reagent`, 102 of the same 229 mutants got one, in
  1.37 s against 2.32 s for 229 snapshots of files only: about 1 s less for each run. The two numbers come from two
  sessions at a similar load.
- Bad, because JFR records only write calls. A file that the JVM renames, moves, copies, creates empty or truncates gets
  no event. `baseline/write-baseline` writes a temporary file and moves it into place, so its result looked like a file
  of another process.
- Bad, because JFR keeps a clock of its own, which drifts from `Instant/now` in a JVM that runs for a long time. The
  guard needed a marker write for each read of the recording, to correct the clock, and time windows with a margin for
  each test and each mutant.
- Bad, because a write in a window could blame a mutant that the cache already held, a write between mutants could name
  no mutant, and the recording grew for the whole run.
- Bad, because it was about 350 lines, against about 160 lines for the chosen guard.

### Only the top level of large ignored folders (rejected)

Rejected because it misses a new file in a subfolder that exists. In the main checkout of the repository, with
`target/`, `node_modules/`, `.shadow-cljs/` and `.cpcache/`, a snapshot of 3,213 files took 17.7 to 19.5 ms. With only
the time of change and the file count of the top level of those folders, it took 6.9 ms. When `target/classes/` exists,
a new `target/classes/app/stale.clj` changes only the time of `target/classes/app/`, so the top level of `target/` stays
the same.

### `git status --porcelain --ignored` as the check (rejected)

Rejected because it does not see a changed file in an ignored folder: it shows `!! target/` before and after. It also
starts a process for each check, which took 30 to 60 ms in the main checkout, about 7 s for 231 mutants.

### A copy of the tree and a child JVM (deferred)

cargo-mutants copies the tree to a temporary folder by default. Stryker runs in a sandbox folder, and in its `inPlace`
mode it warns that it changes your files. mutmut copies the sources to `mutants/` and runs the tests there.

- Good, because a relative path then writes into the copy, and the real files stay safe.
- Good, because the copy costs one step for each run: 0.22 s without `.git/`, `node_modules/`, `target/`,
  `.shadow-cljs/`, `.lsp/` and `.clj-kondo/`, and 2.0 s for all 84 MB without `.git/`.
- Bad, because it breaks the one JVM of ADR 0005. A child JVM adds a start of about 1.6 s for each run. The option `-s`,
  which mutates only the lines changed since a git ref, needs git in the parent.
- Bad, because a file one mutant leaves in the copy changes what the later mutants see.
- Bad, because an absolute path, such as one in the home folder, still reaches the real files.

### Take the temporary folders of the tests from the JDK, together with the snapshot (chosen)

- Good, because it costs nothing at run time, and it removes the cause of the incident.
- Bad, because alone it does not stop a new test that takes its folder from the code under test. The snapshot is for
  that test.

## Consequences

- A run stops early, with exit code 2, when a mutant changes a file in the working directory. The person puts the files
  back and fixes the test, and the next run runs that mutant again.
- A save in an editor during a run stops the run, unless the editor writes only the skipped names. The person runs
  again, and the cache keeps the other outcomes.
- A test that writes the same bytes into the working directory in every run does not stop the run.
- A later change can move the file code in `src/` from `java.io.File` to `java.nio` paths, and give the unit tests an
  in-memory file system. Then fewer tests write to the disk. It does not replace the guard: a test that uses the real
  file system can still write into the working directory.

## Confirmation

- The runner's tests pass, also those for a test that writes or deletes a file only under a mutant, a baseline that
  writes a file each run, a new or deleted folder, a mutant that fails at load and one that does not compile, a mutant
  whose tests still run after the timeout, a failed restore of a mutant that also changed a file, and a cache folder and
  a `.git/` folder inside the working directory.
- The repro of the incident stops at the mutant and names the 7 files. Today `fs/tmp-file-path` is gone, so the repro
  uses `fs/parent`, which returns `nil` under its body mutant. `disk-path` makes the text of `root`, so a `nil` root
  gives a path relative to the working directory, as `java.io.File` did. Note which of these folders exist before the
  run: `.hidden/`, `out/`, `node_modules/`, `node_modules/thing/`, `target/`, `target/classes/`, `target/classes/app/`
  and `.mutate-cache/`. Then add the scratch test `test/cljs_patrol/scratch_nil_root_test.clj`. Its fixture deletes the
  temporary folder that the test makes:

  ```clojure
  (ns cljs-patrol.scratch-nil-root-test
    (:require
     [cljs-patrol.fs :as fs]
     [cljs-patrol.temp-dir :as temp-dir]
     [clojure.test :refer [deftest is use-fixtures]])
    (:import
     [java.nio.file FileSystems]))

  (use-fixtures :each temp-dir/delete-created)

  (defn- disk-path [root leaf]
    (fs/path (FileSystems/getDefault) (str root) leaf))

  (defn- scratch-tree []
    (let [root (fs/parent (fs/path (FileSystems/getDefault) (temp-dir/file-path "x")))]
      (doseq [dir ["src" "target/classes/app" "node_modules/thing" ".hidden" "out"]]
        (fs/create-dirs! (disk-path root dir)))
      (doseq [leaf ["src/kept.clj" "src/kept.cljs" "src/skipped.txt"
                    "target/classes/app/stale.clj" "node_modules/thing/vendored.cljs"
                    ".hidden/secret.clj" "out/compiled.cljs"]]
        (with-open [w (fs/text-writer (disk-path root leaf))]
          (.write w "(ns x)")))
      root))

  (deftest tree-is-listed
    (let [root (scratch-tree)]
      (try
        (is (= 2 (count (fs/source-paths root #{".clj" ".cljs"}))))
        (finally (fs/delete-recursively! root)))))
  ```

  Run `clojure -M:test:mutate cljs-patrol.fs -t cljs-patrol.scratch-nil-root-test -d parent`. The run stops at
  `parent/85:3/body->nil`, names the 7 files first, then the new folders, and exits with 2. Then delete the 7 files,
  each folder of the list above that did not exist before the run, deepest first, and the scratch test. Delete the cache
  file `.mutate-cache/cljs-patrol.fs.edn` if it exists, and `.mutate-cache/` if it did not exist before the run. Do not
  delete `src/`, or a folder that existed before the run.

- A run with the guard on `cljs-patrol.groups.reagent` and on `cljs-patrol.fs` takes less than 2 times as long as the
  same run without it, on a machine with no other heavy load.

## Revisit when

- A run deletes or changes a file that the guard can only report. Then make the copy of the tree and the child JVM.
- A run with the guard takes 2 times as long as the run without it, or more.
- A run stops at a path that no test fixture writes, and the person finds that an editor or another tool wrote it. Then
  add its name or folder to the skipped list, or give the guard a way to tell such files from the files of the tests.

## More Information

- The code:
  [`dev/cljs_patrol/dev/mutate/guard.clj`](https://github.com/olecve/cljs-patrol/blob/c72792b2ccad935207813dbfaec48e953aab07cc/dev/cljs_patrol/dev/mutate/guard.clj),
  and the stop in the mutant loop `run-all` of
  [`dev/cljs_patrol/dev/mutate/run.clj`](https://github.com/olecve/cljs-patrol/blob/c72792b2ccad935207813dbfaec48e953aab07cc/dev/cljs_patrol/dev/mutate/run.clj).
- The test helper: [`test/cljs_patrol/temp_dir.clj`](../../test/cljs_patrol/temp_dir.clj).
- JDK. [`jdk.jfr.Recording`](https://docs.oracle.com/en/java/javase/22/docs/api/jdk.jfr/jdk/jfr/Recording.html), JDK 22.
- JDK-8194154. [System property user.dir should not be changed](https://bugs.openjdk.org/browse/JDK-8194154).
- cargo-mutants. [Copying the tree](https://github.com/sourcefrog/cargo-mutants/blob/v27.1.0/book/src/build-dirs.md), at
  v27.1.0.
- Stryker.
  [`sandbox.ts`](https://github.com/stryker-mutator/stryker-js/blob/v10.1.1/packages/core/src/sandbox/sandbox.ts#L55-L60),
  at v10.1.1.
- mutmut. [`harness.py`](https://github.com/boxed/mutmut/blob/3.8.1/src/mutmut/runners/harness.py#L107), at 3.8.1.
- The pull request: [olecve/cljs-patrol#176](https://github.com/olecve/cljs-patrol/pull/176)
