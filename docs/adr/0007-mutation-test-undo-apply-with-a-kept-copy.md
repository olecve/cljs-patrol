# ADR 0007: Undo An Applied Mutant With A Kept Copy, Not Git

## Status

Accepted, 2026-11-21

## Context

A gap is a mutant that survived. A test that closes a gap must fail with that mutant in place. To see that, the author
must write the mutant into the source file, run the test, and put the code back. Patrol Backlog #80 asked for a command
that does this.

The first version of `--apply` (commit 790343a, pull request 168) used git to undo the change. It refused a file with
uncommitted changes, and printed a `git restore` command. The first review of the pull request found these problems:

- The main use of the runner is `-s main` on code that is not committed yet. `--apply` refused exactly those files.
- The printed `git restore` command used a path from the working directory. It failed when the file was in a different
  repository.
- A file that git ignores passed the check, but `git restore` cannot bring it back.
- When `git status` failed, the check passed and the file was written. This was a bug of that version: it did not read
  the exit code of `git status`.
- The check repeated the git calls of `lines-since`. This was also a fault of that version, not of git.

## Decision

`--apply` keeps the text of the file before the change, and `--restore` writes that text back. Git is not used.

- **The kept copy.** `--apply 'ROW:COL:OPERATOR'` writes `{:file … :original … :applied …}` to `.mutate-applied.edn` in
  the directory the command runs from. The repository's `.gitignore` lists that file.
- **The order of the writes.** `--apply` writes the kept copy first, then the source. When the write of the source
  fails, it removes the kept copy.
- **A safe write.** `--apply` and `--restore` write the new text to a temporary file beside the source. Then they move
  that file over the source in one step. A failed write leaves the source as it was.
- **The right mutant.** The position names the operator. When the file changed and another operator is at that place,
  `--apply` stops. Before it writes, it checks that the operator is at the computed place in the text.
- **One mutant at a time.** A second `--apply`, or a normal run, stops while a mutant is applied.
- **No lost edit.** `--restore` writes the original back only when the file still holds the applied text. When the file
  already holds the original, it only removes the kept copy. In every other case it changes nothing, and it says where
  the kept copy is.
- **A clear command line.** `--restore` needs no target. `--apply` and `--restore` refuse the options of a run (`-t`,
  `-d`, `-s`, `--timeout`, `-r`, `--html`, `-a`). A kept copy that holds anything else stops every command and says to
  delete it.

## Options

### Undo with git (rejected)

- Good, because it needs no file of its own.
- Bad, because it refuses code that is not committed yet, which is the main case.
- Bad, because `git restore` cannot bring back a file that git ignores, or one in a different repository from a path of
  this one.

### Keep the copy in `target/` (rejected)

Rejected because `clojure -T:build uber` deletes `target/`, and then the only copy of uncommitted text is lost. The
second review of pull request 168 found this.

### Keep the copy in the directory the command runs from (chosen)

- Good, because it works on any file, with or without git, and on code that is not committed yet.
- Good, because `--restore` can check that nothing changed since the apply.
- Bad, because the kept copy is found by a relative path. A command that runs from another directory does not see an
  applied mutant, and `.gitignore` lists the file only for this repository.
- Bad, because a person can delete the kept copy by hand while a mutant is applied. The messages say what to do then.

## Consequences

- The page prints the `--apply` command for each gap that the tests ran. The console prints one example.
- The printed position is quoted, `--apply '2:32:>'`, because `>` and `<=` break the command in any shell, and `?`, as
  in `nil?`, breaks it in zsh.
- The command works on a branch before its commits, which is when the author writes the new test.

## Confirmation

- `clojure -M:test -n cljs-patrol.dev.mutate-test` passes. Its tests keep Windows line endings on `--apply`, clear the
  kept copy when the file was put back by hand, and leave the file as it was, with no kept copy, when the write in
  `--apply` fails.

## Revisit when

- The runner moves to a project of its own (Patrol Backlog #85). Then the place of the kept copy must follow that
  project's working directory and ignore rules.
- A cljs-patrol editor plugin can apply and undo a mutant. Then the editor's undo can replace the kept copy.

## More Information

- The code: `apply-mutant`, `restore-mutant`, `applied-mutant`, `write-whole`, `run-cli`, `usage-errors` and
  `applied-file` in [`dev/cljs_patrol/dev/mutate.clj`](../../dev/cljs_patrol/dev/mutate.clj)
- How to use it: the "Watch a new test fail" section of [`docs/mutation-testing.md`](../mutation-testing.md)
- The pull request: [olecve/cljs-patrol#168](https://github.com/olecve/cljs-patrol/pull/168)
- The first version, with git:
  [790343a](https://github.com/olecve/cljs-patrol/commit/790343ae47446609949e07ff4dc4cce672787e33)
- The kept copy and `--restore`:
  [ea3677f](https://github.com/olecve/cljs-patrol/commit/ea3677fa881b17a4bb364c7eacb8c3a79a35b482)
- The copy out of `target/`, and the quoted command:
  [bd9dc31](https://github.com/olecve/cljs-patrol/commit/bd9dc31fdff54ac6a493c2e3117707442993e432)
- The safe write and the operator check:
  [66a2519](https://github.com/olecve/cljs-patrol/commit/66a2519ce966baf0499dd95be381de5dd35c1aaf)
