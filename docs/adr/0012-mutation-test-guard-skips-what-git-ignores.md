# ADR 0012: Skip What Git Ignores In The Mutation Test File Guard

## Status

Accepted, 2026-11-23 (proposed 2026-11-23)

## Context

This repository has a dev-only mutation runner. It checks the tests. It plants one small change, a mutant, in the code
under test, and runs the tests. It runs every mutant in one JVM, in the directory where the command starts: the working
directory. Before the first mutant, it runs the tests once on the unchanged code. This is the baseline.

A test can write, change or delete a file in the working directory, and the runner cannot undo that. So the runner has a
file guard. After each mutant, the guard takes a snapshot: the path of every file and folder under the working
directory, and the size and time of change of each file. It compares the snapshot with the reference, the last snapshot
with no change. When they differ, the run stops and names the paths.
[ADR 0011](0011-mutation-test-stop-when-a-test-changes-a-file.md) records the guard.

This record partially supersedes ADR 0011. It replaces only the part that says which paths the snapshot skips. The rest
of ADR 0011 stays.

Until now, the snapshot skipped the `.git/` folder of the working directory, the cache folder of the run, and a hand
list in the guard:

- the folders `.lsp/`, `.clj-kondo/.cache/`, `.clj-kondo/imports/`, `.clj-kondo/inline-configs/`, `.cpcache/`,
  `.shadow-cljs/`, `.idea/` and `.calva/`
- the names `*.swp`, `*.swo`, `4913`, `.#*`, `#*#`, `*~`, `.DS_Store` and `.nrepl-port`, at any depth

The snapshot walked every other folder, also the folders that git ignores, such as `node_modules/` and `target/`. These
facts follow:

- **Cost.** In a worktree of this repository after `npm ci` and `clojure -T:build uber`, the snapshot held 3,908 files
  and folders, and 3,467 of them were in `node_modules/` and `target/`. One snapshot took 33 ms. On
  `cljs-patrol.groups.reagent`, the 229 snapshots took 7.8 s of a 10.4 s run. The run without the guard took 2.5 s.
- **False stops.** A tool that writes a new folder during a run stops the run when the folder is not on the hand list.
  The fix was to add the folder to the list, one tool at a time.
- **What git ignores.** A file that git ignores is not part of the project. Git does not commit it. The project's
  `.gitignore` names the folders of its build tools, such as `target/`, `out/` and `node_modules/`.

## Decision

The snapshot skips what git ignores. The guard asks git once which paths it ignores, and asks git again only about new
paths that a snapshot finds.

- **The list.** After the baseline, the guard runs
  `git --no-optional-locks status --porcelain=v1 -z --ignored=matching --untracked-files=normal --no-renames -- .` in
  the working directory. The option `--no-optional-locks` stops `git status` from writing `.git/index`. Git gives each
  path relative to the root of the repository, so the guard removes the prefix that `git rev-parse --show-prefix` gives.
  Then the paths are relative to the working directory, also when the working directory is a subfolder of the
  repository. Every later snapshot skips these paths. When git ignores a whole folder, git gives the folder once, and
  the snapshot does not enter it.
- **Why `git status`.** `git ls-files --others --ignored --exclude-standard --directory` also gives a folder whose files
  are all ignored, even when no rule ignores the folder itself. A new file in that folder would then go unseen. With
  `--ignored=matching`, `git status` gives such a folder's ignored files, not the folder.
- **New paths.** When a snapshot finds added paths, and git can tell which paths it ignores, the guard gives the added
  paths to `git check-ignore -z --stdin`. Git matches each path against the ignore rules, also a path in a folder that a
  rule ignores. A tracked file is never ignored. A new path that git ignores is no change, and neither is a new folder
  that holds one. The guard adds the ignored paths to the skipped paths. It removes them, and the folders that now hold
  a skipped path, from the new snapshot and from the changes, with the same rules as the walk. It does not walk again.
  Later snapshots skip them too. When no change is an added path, the guard does not start git. Thus a new `target/` in
  a fresh clone does not stop the run. A changed or deleted path is not given to git, since the baseline snapshot held
  it, so git did not ignore it then.
- **The snapshot before the baseline.** It skips only `.git/`, the cache folder, and the folders and names of editors
  below. The guard compares it with the snapshot after the baseline to find the files that the baseline wrote. A path
  that is only in the first one, such as a file in `node_modules/`, counts as deleted there, and the guard drops it.
- **What stays skipped by hand.** The `.git/` folder of the working directory and the cache folder of the run. The
  folders of editors, IDEs and their language servers: `.lsp/`, `.clj-kondo/.cache/`, `.clj-kondo/imports/`,
  `.clj-kondo/inline-configs/`, `.idea/` and `.calva/`. The names of the hand list. A person's own setup writes these
  while a run goes on. The project's `.gitignore` may not name them, and the person's global ignore file may not either.
  `.cpcache/` and `.shadow-cljs/` leave the hand list: they are build folders, and the project's `.gitignore` names
  them.
- **Outside git.** When the working directory is not in a git work tree, or git fails, the guard skips the whole hand
  list as before, and asks git nothing about new paths. The run prints one note:
  `Note: git cannot tell which files it ignores here, so the file guard also checks them, such as target/.`
- **An ignored working directory.** When an ignore rule matches the working directory itself, or a folder above it, git
  ignores every new path under it. The guard treats this run as a run outside git. It finds the case with
  `git check-ignore -q --no-index .` in the working directory: exit code 0 means ignored. Without `--no-index`, git
  calls a folder that holds a tracked file not ignored, also when a rule matches it, and it does not match a rule such
  as `build/` against the working directory `build` itself.
- **Folders that hold a skipped path.** A folder that holds a skipped path is not in the snapshot itself, but its other
  files are. This rule was there for the folders of the hand list. It now applies to every skipped path.

## Options

### Skip what git ignores (chosen)

- Good, because the snapshot gets small. In the same worktree, it held 441 files and folders, and one snapshot took 7
  ms.
- Good, because the run gets faster. Each time is the median of 3 runs in one JVM, at a load average of 4.4 to 5.5 on 8
  cores:

  | Target                       | Mutants | Without the guard | Hand list                  | What git ignores          |
  | ---------------------------- | ------- | ----------------- | -------------------------- | ------------------------- |
  | `cljs-patrol.groups.reagent` | 229     | 2.46 s            | 10.38 s, 4.2 times as long | 4.70 s, 1.9 times as long |
  | `cljs-patrol.fs`             | 27      | 0.29 s            | 1.32 s, 4.5 times as long  | 0.57 s, 1.9 times as long |

  The reagent runs leave out the function `element-children` of `cljs-patrol.groups.reagent`, since its mutant at line
  302 of `src/cljs_patrol/groups/reagent.clj` loops when the machine is busy.

- Good, because a new tool folder that git ignores does not stop the run. Nobody needs to add it to a list.
- Good, because a new folder that git ignores, such as `target/` in a fresh clone, does not stop the run either.
- Good, because the project's `.gitignore` already names these folders. The person keeps one list, not two.
- Bad, because the guard does not see a write into a file that git ignores. That is build output, but also a local
  `.env` file or a local config file. A test that overwrites or deletes such a file goes unseen, and the file is lost.
- Bad, because a test that writes a new `.gitignore` changes what git ignores. With a rule such as `*`, it hides itself
  and the new files beside it.
- Bad, because a run starts two git processes after the baseline, about 25 ms in this worktree, and one more, about 10
  ms, for each snapshot that finds new paths.
- Bad, because the snapshot before the baseline still walks the ignored folders once: 33 ms in this worktree.
- Bad, because the folders and names of editors stay on a hand list. A new editor folder that git does not ignore still
  stops the run.

### Keep the hand list (rejected)

- Good, because it starts no git process.
- Good, because it sees a write into a file that git ignores, outside the folders of the list.
- Bad, because the guard walks every ignored folder after each mutant. In the worktree above, the guard made the reagent
  run 4.2 times as long as the run without it, against 1.9 times when it skips what git ignores.
- Bad, because each new tool folder stops the run until somebody adds it to the list.

### `git status --porcelain` as the check (rejected)

ADR 0011 rejected this because `git status` shows `!! target/` before and after a change in an ignored folder. That
reason no longer holds: the chosen option does not look into ignored folders either. The option still loses, for other
reasons:

- Bad, because `git status` shows an untracked file as `??` before and after its bytes change. It misses a change of a
  new source file that is not committed yet.
- Bad, because it shows a changed tracked file as ` M` before and after its bytes change again. It misses a second
  change of a file that the person changed before the run.
- Bad, because it starts a process for each mutant: 22 ms in this worktree, about 5 s for 229 mutants. The 229 snapshots
  of the chosen option took 2.2 s.

## Consequences

- A run in a project with large ignored folders, such as `node_modules/` or `target/`, gets about twice as fast.
- A build tool or another tool that writes into an ignored folder during a run does not stop the run.
- A test that writes into an ignored file goes unseen. A person who keeps a local `.env` or config file in the working
  directory can lose it to such a test.
- A run outside git works as before, and prints one note.

## Confirmation

- The runner's tests pass. Tests with a real git repository, made with `git init`, cover these cases: an ignored folder
  that exists at the start, a new ignored folder in a fresh clone, a new path that git ignores beside one that it does
  not, a new folder that holds only ignored paths, an existing folder that gets a new ignored file, a change that adds
  no path, the list from a subfolder, and a tracked file that an ignore rule matches. Other tests cover a working
  directory that git ignores, also one in an ignored folder that holds a tracked file, a new file there that stops the
  run, a directory outside git, and a run in git whose baseline writes into an ignored `target/` with other bytes for
  each mutant.
- The repro of an incident still stops. In that incident, the mutant that made `fs/tmp-file-path` return `nil` gave a
  test helper the root folder `nil`. `(java.io.File. nil "src")` is the relative path `src`, so the helper wrote its 7
  fixture files into the working directory. `fs/tmp-file-path` is gone, so the repro uses `fs/parent`, which returns
  `nil` under its body mutant. `disk-path` makes the text of `root`, so a `nil` root gives a path relative to the
  working directory, as `java.io.File` did. Run it in a worktree with `node_modules/` and `target/`. First note which of
  these folders exist: `.hidden/`, `out/`, `node_modules/thing/`, `target/classes/app/` and `.mutate-cache/`. Then add
  the scratch test `test/cljs_patrol/scratch_nil_root_test.clj`. Its namespace `cljs-patrol.scratch-nil-root-test`
  requires `cljs-patrol.fs` as `fs`, `cljs-patrol.temp-dir` as `temp-dir` and `deftest` and `is` from `clojure.test`,
  and imports `java.nio.file.FileSystems`. Its fixture `temp-dir/delete-created` deletes the temporary folder that the
  test makes:

  ```clojure
  (defn- disk-path [root leaf]
    (fs/path (FileSystems/getDefault) (str root) leaf))

  (defn- scratch-tree []
    (let [root (fs/parent (fs/path (FileSystems/getDefault) (temp-dir/file-path "x")))]
      (doseq [leaf ["src/kept.clj" "src/kept.cljs" "src/skipped.txt" ".hidden/secret.clj"
                    "node_modules/thing/vendored.cljs" "target/classes/app/stale.clj" "out/compiled.cljs"]]
        (let [p (disk-path root leaf)]
          (fs/create-dirs! (.getParent p))
          (with-open [w (fs/text-writer p)]
            (.write w "(ns x)"))))
      root))

  (deftest tree-is-listed
    (let [root (scratch-tree)]
      (try
        (is (= 2 (count (fs/source-paths root #{".clj" ".cljs"}))))
        (finally (fs/delete-recursively! root)))))
  ```

  Run `clojure -M:test:mutate cljs-patrol.fs -t cljs-patrol.scratch-nil-root-test -d parent`. The run stops at
  `parent/85:3/body->nil` and exits with 2. It names the 4 files that git does not ignore: `src/kept.clj`,
  `src/kept.cljs`, `src/skipped.txt` and `.hidden/secret.clj`, and the new folder `.hidden/`. It does not name the other
  3 files, since the `.gitignore` of this repository ignores `node_modules/`, `target/` and `out`. Then delete the 7
  files, each folder of the list above that did not exist before the run, deepest first, and the scratch test. Delete
  `.mutate-cache/cljs-patrol.fs.edn` if it exists, and `.mutate-cache/` if it did not exist before the run. Do not
  delete `src/`, or a folder that existed before the run.

- A run with the guard on `cljs-patrol.groups.reagent` and on `cljs-patrol.fs` takes less than 2 times as long as the
  same run without it, in a worktree with `node_modules/` and `target/`, on a machine with no other heavy load.

## Revisit when

- A test overwrites or deletes a file that git ignores, and a person loses work. Then make the guard look into ignored
  files that are not build output, or give the tests a copy of the tree.
- A run with the guard takes 2 times as long as the run without it, or more.
- A run stops at a path that an editor or another tool wrote, and git does not ignore it. Then add it to the hand list,
  or tell the person to add it to their global ignore file.

## More Information

- The code: [`dev/cljs_patrol/dev/mutate/guard.clj`](../../dev/cljs_patrol/dev/mutate/guard.clj), the git calls in
  [`dev/cljs_patrol/dev/mutate/git.clj`](../../dev/cljs_patrol/dev/mutate/git.clj), and the start of the guard in
  `mutate-source` of [`dev/cljs_patrol/dev/mutate.clj`](../../dev/cljs_patrol/dev/mutate.clj).
- The record of the guard: [ADR 0011](0011-mutation-test-stop-when-a-test-changes-a-file.md).
- git. [`git status --ignored`](https://github.com/git/git/blob/v2.47.0/Documentation/git-status.txt), at v2.47.0.
- git. [`git check-ignore`](https://github.com/git/git/blob/v2.47.0/Documentation/git-check-ignore.txt), at v2.47.0.
- git. [`git ls-files --directory`](https://github.com/git/git/blob/v2.47.0/Documentation/git-ls-files.txt), at v2.47.0.
- The measurements used git 2.56.1 and JDK 22.
- The pull request: [olecve/cljs-patrol#177](https://github.com/olecve/cljs-patrol/pull/177)
