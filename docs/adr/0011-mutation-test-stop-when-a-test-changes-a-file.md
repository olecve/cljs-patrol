# ADR 0011: Stop A Mutation Run When A Test Changes A File

## Status

Proposed, 2026-11-23

## Context

This repository has a dev-only mutation runner ([ADR 0005](0005-mutation-testing-in-one-jvm.md)). A mutation runner
checks the tests. It plants one small change, a mutant, in the code under test, and runs the tests. When a test fails,
the mutant is killed. When all tests pass, the mutant survived, and no test checks that code.

- **One JVM.** This runner runs every mutant in one JVM, in the directory where the command starts. It loads the changed
  form into the running JVM, runs the tests, and then loads the original form again. It does not put back a file that
  the tests changed.
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

After each mutant whose tests ran, the runner takes a snapshot of the files in the working directory and compares it
with the snapshot after the baseline. When a file is new, changed or deleted, the run stops. The tests take their
temporary folders from the JDK, not from the code under test.

- **Snapshot.** The path, the size and the time of change of every file under the directory the command runs from. The
  snapshot does not enter the `.git/` folder of that directory, the cache folder of the run, which is `.mutate-cache/`
  by default, and the folders that tools write while a run goes on: `.lsp/`, `.clj-kondo/.cache/`, `.cpcache/` and
  `.shadow-cljs/`. It also leaves out the swap, lock and backup files of editors: `*.swp`, `*.swo`, `.#*`, `*~` and
  `4913`. Other folders that git ignores, such as `target/` and `node_modules/`, count. A file that is deleted while the
  snapshot is taken is left out, and a folder that cannot be read counts as one entry, "unreadable". When two snapshots
  are equal, the comparison ends at once.
- **Which mutants.** Every mutant whose tests ran gets a snapshot. A mutant that cannot be read or compiled, or that
  fails while it loads, runs no test and gets none.
- **Files that the baseline wrote.** A file that the baseline added or changed counts as changed only when its bytes
  change, because its test writes it again for each mutant. When its bytes change, the message says that the baseline
  wrote it too.
- **Stop.** The run stops with the id of the mutant that just ran and up to 10 files, and exits with 2. It says that it
  did not undo the files. When the tests of that mutant still run after the timeout, or its restore failed, the message
  says that too. The cache has no outcome of that mutant, so the next run runs it again.
- **Tests.** A new test helper, `cljs-patrol.temp-dir`, makes each temporary folder with the JDK call
  `Files/createTempDirectory`. The folder is new, empty, and in the OS temporary folder. `fs_test.clj` and the four
  other test files above use it. `fs/tmp-dir` and `fs/tmp-file-path` are deleted, since nothing calls them now.

## Options

### A snapshot after every mutant whose tests ran (chosen)

- Good, because it finds every new, changed or deleted file in every folder, the folders that git ignores too, however
  the file was made: a write, a rename, a move, a copy, an empty create or a truncate.
- Good, because the file that stops the run belongs to the mutant that just ran, so the message always names it.
- Good, because the guard is small: about 150 lines.
- Good, because the cost stays under 2 times the run without it. The times are medians of 3 runs in the same JVM, with
  and without the guard, on JDK 22, in a worktree of this repository whose snapshot holds 390 files, at a load average
  of 5.2 to 7.0. On `cljs-patrol.groups.reagent`, with `-d` for every definition but `element-children`, whose mutant at
  line 302 loops when the machine is busy, the 229 mutants took 229 snapshots in 2.32 s, about 10 ms each. The run went
  from 3.57 s to 6.06 s, 1.70 times as long. On `cljs-patrol.fs`, the 27 mutants took 27 snapshots in 0.29 s, and the
  run went from 0.40 s to 0.69 s, 1.74 times as long.
- Good, because it found the real case: with the old `fs_test.clj`, the run stopped at `tmp-file-path/55:3/body->nil`
  and named the 7 files. On today's tree, a scratch test does the same with `fs/parent-dir`, as Confirmation describes.
- Bad, because it only reports. A deleted file that git does not hold is lost.
- Bad, because it does not see a file outside the working directory, such as one in the home folder.
- Bad, because another process that changes a file during the run, such as an editor that saves a source file, also
  stops the run, and the message names the mutant that just ran.

### A snapshot only after mutants whose tests write, found with Java Flight Recorder (rejected)

Java Flight Recorder (JFR), the event recorder of the JDK, records a `jdk.FileWrite` event for each write call of the
JVM. In the baseline, a test that wrote a file marked its mutants for a snapshot, and a second recording during the
mutants told a write of this run from a write of another process.

- Good, because fewer mutants get a snapshot. On `cljs-patrol.groups.reagent`, 102 of the same 229 mutants got one, in
  1.37 s against 2.32 s: about 1 s less for each run. The two numbers come from two sessions at a similar load.
- Bad, because JFR records only write calls. A file that the JVM renames, moves, copies, creates empty or truncates gets
  no event. `baseline/write-baseline` writes a temporary file and moves it into place, so its result looked like a file
  of another process.
- Bad, because JFR keeps a clock of its own, which drifts from `Instant/now` in a JVM that runs for a long time. The
  guard needed a marker write for each read of the recording, to correct the clock, and time windows with a margin for
  each test and each mutant.
- Bad, because a write in a window could blame a mutant that the cache already held, a write between mutants could name
  no mutant, and the recording grew for the whole run.
- Bad, because it was about 350 lines, against about 150 lines for the chosen guard.

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

- A run stops early, with exit code 2, when the tests of a mutant change a file in the working directory. The person
  puts the files back and fixes the test, and the next run runs that mutant again.
- A save in an editor during a run stops the run, unless the editor writes only its swap, lock or backup files. The
  person runs again, and the cache keeps the other outcomes.
- A test that writes the same bytes into the working directory in every run does not stop the run.
- A later change can move the file code in `src/` from `java.io.File` to `java.nio` paths, and give the unit tests an
  in-memory file system. Then fewer tests write to the disk. It does not replace the guard: a test that uses the real
  file system can still write into the working directory.

## Confirmation

- The runner's tests pass, also those for a test that writes or deletes a file only under a mutant, a baseline that
  writes a file each run, a mutant that fails at load, a mutant whose tests still run after the timeout, and a failed
  restore of a mutant that also changed a file.
- The repro of the incident stops at the mutant and names the 7 files. Today `fs/tmp-file-path` is gone, so the repro
  uses `fs/parent-dir`, which returns `nil` under its body mutant. Add a scratch test,
  `test/cljs_patrol/scratch_nil_root_test.clj`, whose `scratch-tree` helper is the one of the old `fs_test.clj` with its
  root from `(fs/parent-dir (temp-dir/file-path "x"))`. Run
  `clojure -M:test:mutate cljs-patrol.fs -t cljs-patrol.scratch-nil-root-test -d parent-dir`. The run stops at
  `parent-dir/19:3/body->nil`, names the 7 files and exits with 2. Then delete the 7 files, their folders and the
  scratch test.
- A run with the guard on `cljs-patrol.groups.reagent` and on `cljs-patrol.fs` takes less than 2 times as long as the
  same run without it, on a machine with no other heavy load.

## Revisit when

- A run deletes or changes a file that the guard can only report. Then make the copy of the tree and the child JVM.
- A run with the guard takes 2 times as long as the run without it, or more.
- Saves in an editor stop runs often. Then the guard needs a way to tell those files from the files of the tests.

## More Information

- The code: [`dev/cljs_patrol/dev/mutate/guard.clj`](../../dev/cljs_patrol/dev/mutate/guard.clj), and the stop in the
  mutant loop `run-all` of [`dev/cljs_patrol/dev/mutate/run.clj`](../../dev/cljs_patrol/dev/mutate/run.clj).
- The test helper: [`test/cljs_patrol/temp_dir.clj`](../../test/cljs_patrol/temp_dir.clj).
- JDK. [`jdk.jfr.Recording`](https://docs.oracle.com/en/java/javase/22/docs/api/jdk.jfr/jdk/jfr/Recording.html), JDK 22.
- JDK-8194154. [System property user.dir should not be changed](https://bugs.openjdk.org/browse/JDK-8194154).
- cargo-mutants. [Copying the tree](https://github.com/sourcefrog/cargo-mutants/blob/v27.1.0/book/src/build-dirs.md), at
  v27.1.0.
- Stryker.
  [`sandbox.ts`](https://github.com/stryker-mutator/stryker-js/blob/v10.1.1/packages/core/src/sandbox/sandbox.ts#L55-L60),
  at v10.1.1.
- mutmut. [`harness.py`](https://github.com/boxed/mutmut/blob/3.8.1/src/mutmut/runners/harness.py#L107), at 3.8.1.
