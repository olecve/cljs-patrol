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
  it records which tests call each function. Each mutant then runs only the tests that call its function.
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
change of the property does not change how a relative path resolves. `deps.edn` does not pin a JDK; the measurements
below use JDK 22.

## Decision

The runner compares a snapshot of the files in the working directory after the mutants whose tests could write a file,
and stops the run when a file is new, changed or deleted. The tests take their temporary folders from the JDK, not from
the code under test.

- **Snapshot.** The path, the size and the time of change of every file under the directory the command runs from. The
  snapshot does not enter the `.git/` folder of that directory, the cache folder of the run, which is `.mutate-cache/`
  by default, and the folders that tools write while a run goes on: `.lsp/`, `.clj-kondo/.cache/`, `.cpcache/` and
  `.shadow-cljs/`. Other folders that git ignores, such as `target/` and `node_modules/`, count. The reference is the
  snapshot after the baseline. A file that is deleted while the snapshot is taken is left out, and a folder that cannot
  be read counts as one entry, "unreadable".
- **Which mutants.** In the baseline, the runner records each test that may write. A test may write when Java Flight
  Recorder (JFR), the event recorder of the JDK, records a `jdk.FileWrite` event of this JVM while the test runs, or
  when the names directly in the working directory before and after the test are not the same. The second check finds a
  folder that the test makes, or a file that a process it starts writes, which JFR does not record. A mutant gets a
  snapshot when a test that may write runs for it. A write that falls in no test, such as one in a fixture, makes every
  test one that may write. JFR uses a clock of its own, so before it reads the writes, the runner writes one marker file
  and moves the JFR times by the difference. Every mutant gets a snapshot when the baseline left a file changed, or when
  the baseline had to run without recording. That happens when the recording makes the tests fail or run past the
  timeout. A file that the baseline wrote counts as changed only when its bytes change, because its test writes it again
  for each mutant.
- **Writes during the mutants.** A second JFR recording runs during the mutants, and the runner notes when the tests of
  each mutant ran. A new or changed file stops the run only when this JVM wrote that file. Another file, such as a
  source file that an editor saved, prints one warning, "FILE changed during the run, but not by this run's tests.", and
  the snapshot takes its new state, so it does not warn again. JFR records no delete, so a deleted file always stops the
  run.
- **No JFR.** On a JDK without JFR, every mutant gets a snapshot, every new or changed file stops the run, and the run
  says so once.
- **Stop.** The run stops with the id of the mutant whose tests wrote the file, and up to 10 files, and exits with 2. It
  says that it did not undo the files. When the restore of the mutant also failed, the message says that too. The cache
  has no outcome of the mutant that the run stopped at, so the next run runs it again.
- **Last look.** One more snapshot after the last mutant, and at each other early stop, finds a file that a mutant
  without a snapshot changed. The message names the mutant when JFR saw its tests write the file. When it cannot name
  the mutant, it tells the person to run again with `--fresh` and `-d`, one definition at a time. Without `--fresh`, the
  next run reuses every cached outcome and runs no test.
- **Tests.** A new test helper, `cljs-patrol.temp-dir`, makes each temporary folder with the JDK call
  `Files/createTempDirectory`. The folder is new, empty, and in the OS temporary folder. `fs_test.clj` and the four
  other test files above use it.

## Options

### A snapshot of the working directory after each mutant whose tests could write (chosen)

- Good, because it finds a new, changed or deleted file in every folder, the folders that git ignores too.
- Good, because the cost stays small. The times are medians of 3 runs in the same JVM, with and without the guard, on
  JDK 22, at a load average of 4.7 to 5.7. On `cljs-patrol.groups.reagent`, the run left out `element-children`, whose
  mutant at line 302 loops when the machine is busy. Then 102 of 229 mutants get a snapshot. Their tests include
  `heads-referred-as-macros-are-read`, which writes a project to a temporary folder. The snapshots took 1.37 s, and the
  run went from 4.27 s to 5.77 s, 1.35 times as long. The JFR recording during the mutants added about 0.02 s: the run
  took 5.75 s without it. On `cljs-patrol.fs`, the 27 mutants and the last look took 28 snapshots in 0.41 s, and the run
  went from 0.87 s to 1.26 s. The record of the tests that write adds about 15 ms to the baseline, and the first start
  of JFR takes about 150 ms.
- Good, because another process does not change which tests write. With a shell loop that made and deleted a file in the
  OS temporary folder every 50 ms, reagent still had 102 mutants with a snapshot in 2 runs.
- Good, because another process that writes into the working directory, such as an editor or a linter, gives a warning,
  not a stop.
- Good, because it found the real case: with the old `fs_test.clj`, the run stops at `tmp-file-path/55:3/body->nil` and
  names the 7 files. After `fs/tmp-file-path` was deleted, a test that took its root from `fs/parent-dir` in the same
  way stops at `parent-dir/19:3/body->nil` and names the same 7 files.
- Bad, because it only reports. A deleted file that git does not hold is lost.
- Bad, because it does not see a file outside the working directory, such as one in the home folder.
- Bad, because a test that wrote nothing in the baseline gets no snapshot after its mutants. The last look then finds
  the file, but not the mutant.
- Bad, because JFR does not record a new folder, a deleted file, or a file that another process writes. A test that does
  only that in the OS temporary folder, such as one that runs `git` there, gets no snapshot after its mutants. A file
  that a process started by a test writes into the working directory under a mutant gives only a warning.
- Bad, because it uses JFR, which a JDK without JFR does not have.

### A snapshot after every mutant (rejected)

Rejected because it costs about 2.3 times as much on `cljs-patrol.groups.reagent`: 231 snapshots at 13.2 ms each, about
3.0 s against 1.35 s, for the mutants whose tests did not write a file in the baseline. The 13.2 ms is the mean time of
one snapshot of a worktree of this repository with 511 files, on JDK 22, at a load average of about 4.7.

### The time of change of the OS temporary folder as the record of the tests that write (rejected)

Rejected because other processes change it too. A test counted as a test that writes when the time of change of the OS
temporary folder or of the working directory changed while it ran. With the shell loop above, reagent had 102, 227 and
196 mutants with a snapshot in 3 runs, and the run took up to 1.77 times as long. Without the loop, it had 102.

### The names in the OS temporary folder as the record of the tests that write (rejected)

Rejected because a test that makes a temporary folder and deletes it leaves the same names, as `fs_test.clj` did in the
incident. Also, a name that another process makes before the test and deletes after it starts looks like a change. With
the shell loop above, reagent had 165 and 102 mutants with a snapshot in 2 runs.

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

- A run stops early, with exit code 2, when a mutant makes its tests change a file in the working directory. The person
  puts the files back and fixes the test, and the next run runs that mutant again.
- A test that writes a file into the working directory in every run makes every mutant take a snapshot. A test that
  writes the same bytes each time does not stop the run.
- A save in an editor or a linter cache during a run prints a warning, and the run goes on.
- `fs/tmp-dir` and `fs/tmp-file-path` are deleted, since nothing in `src/` or in the tests calls them now.
- A later change can move the file code in `src/` from `java.io.File` to `java.nio` paths, and give the unit tests an
  in-memory file system. Then fewer tests write to the disk, and the guard has less to check. It does not replace the
  guard: a test that uses the real file system can still write into the working directory.

## Confirmation

- The runner's tests pass, also those for a test that writes a file only under a mutant, a test that writes or deletes a
  file only under a mutant that got no snapshot, a baseline that writes a file each run, a write in a fixture, a file
  that another process writes, a failed restore of a mutant that also changed a file, and a JDK without JFR.
- A run on `cljs-patrol.fs` with the old `fs_test.clj` stops at `tmp-file-path/55:3/body->nil` and names the 7 files.
- A run on `cljs-patrol.groups.reagent` stays under 1.5 times its time without the guard. The times are measured on a
  machine with no other heavy load.
- A run on `cljs-patrol.groups.reagent` gives the same number of mutants with a snapshot while another process makes and
  deletes files in the OS temporary folder.

## Revisit when

- A run deletes or changes a file that the guard can only report. Then make the copy of the tree and the child JVM.
- The checks of a run on `cljs-patrol.groups.reagent` take more than half of its time without the guard.
- A test writes into the working directory under a mutant, and the last look finds it, but the mutant got no snapshot.
  Then the record of which tests write needs another signal.

## More Information

- The code: [`dev/cljs_patrol/dev/mutate/guard.clj`](../../dev/cljs_patrol/dev/mutate/guard.clj), with the record of the
  tests that write in the baseline function `coverage`, and the stop in the mutant loop `run-all`, of
  [`dev/cljs_patrol/dev/mutate/run.clj`](../../dev/cljs_patrol/dev/mutate/run.clj), and the JFR recording in
  [`dev/cljs_patrol/dev/mutate/recorder.clj`](../../dev/cljs_patrol/dev/mutate/recorder.clj).
- The test helper: [`test/cljs_patrol/temp_dir.clj`](../../test/cljs_patrol/temp_dir.clj).
- JDK. [`jdk.jfr.Recording`](https://docs.oracle.com/en/java/javase/22/docs/api/jdk.jfr/jdk/jfr/Recording.html), JDK 22.
- JDK-8194154. [System property user.dir should not be changed](https://bugs.openjdk.org/browse/JDK-8194154).
- cargo-mutants. [Copying the tree](https://github.com/sourcefrog/cargo-mutants/blob/v27.1.0/book/src/build-dirs.md), at
  v27.1.0.
- Stryker.
  [`sandbox.ts`](https://github.com/stryker-mutator/stryker-js/blob/v10.1.1/packages/core/src/sandbox/sandbox.ts#L55-L60),
  at v10.1.1.
- mutmut. [`harness.py`](https://github.com/boxed/mutmut/blob/3.8.1/src/mutmut/runners/harness.py#L107), at 3.8.1.
