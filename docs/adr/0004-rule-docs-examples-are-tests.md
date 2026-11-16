# ADR 0004: Run The Rule Docs Examples As Tests

## Status

Accepted, 2026-11-16

## Context

Each rule has a section in `docs/rules/`. The section starts with a key line, which gives the rule key, the tier and a
summary, such as `` `reg-fx-public-fn` · cleanup — … ``. Then the section shows code that the rule flags, the fixed
code, and code that the rule does not flag. People and agents learn a rule from these examples, so an example that is
wrong teaches the wrong rule.

Code examples drift from the tool when nothing checks them. A rule change can make an example false, and the change does
not touch the docs page. A new rule can also ship with no section.

In October 2026 the examples became tests. `test/cljs_patrol/docs_examples_test.clj` read the first code block and the
`**Left alone:**` code block of each rule section, and ran each example in them through the rule. Two problems stayed:

- The test ended an example at each blank line between two top-level forms. So an example of two forms, such as a
  component and the button that renders it, could not have a blank line between the forms. A label comment between two
  forms also split the example. Then the test checked each form alone, and the example did not check what its label
  said.
- Other code blocks did not run: the examples in a `<details>` part or in a config section, the config blocks, and the
  examples on `docs/rules.md`. These blocks could drift.

## Decision

Every code block in the rule docs is a test, and a label comment starts each example.

- **Every block runs.** The test reads each fenced `clojure` and `edn` block of `docs/rules.md`, `docs/rules/*.md` and
  `README.md`. It fails when a block is not an example and not a config. The code blocks in `docs/adr/` are not checked,
  because they show a decision, not the current behavior. The other Markdown files have no such blocks.
- **A label starts an example.** A `;;` comment at the start of a line starts a new example. The example continues to
  the next label, and blank lines in it stay in it. An example with no label ends at a blank line.
- **The label gives the expected result.** A label that starts with `flagged` expects a finding. A label that starts
  with `left alone` or `fix` expects no finding. A label that starts with neither keeps the expected result of the
  example before it. In the first block of a rule section, the `;; flagged` and `;; fix` markers are the labels.
- **A config line sets the config.** A `;; config {…}` line in a block gives the config that every example of the block
  runs under. In a rule section, the config lines of the first block and of the Left alone block apply to both blocks.
  Two config lines merge: maps merge at every depth, and vectors join. A config line is not a label.
- **A form group must use a name from above.** A blank line between top-level forms splits an example into form groups.
  Each form group after the first must use a name that an earlier form group defines. If it does not, it is a different
  example with no label, and the test fails.
- **A config block must be a config.** A block that is one map with keyword keys, or an `edn` block, is a config. Each
  key must name a group or a top-level config key, a `:fail-on` value must name tiers or rules, and no group may refuse
  the config. These checks cannot see an option that a group ignores, such as a wrong key inside the map of a group. The
  test runs the same checks on each `;; config` line.

An example block:

```clojure
;; config {:a11y {:component-aliases {my.ui/popover-content :dialog}}}
;; left alone: the map that the let binds names the popover
(defn notifications-popover []
  (let [content-props {:aria-label "Notifications"}]
    [ui/popover-content content-props
     [notification-list]]))

;; flagged: the map that the let binds has no :aria-label
(defn alerts-popover []
  (let [content-props {:side :top}]
    [ui/popover-content content-props
     [alert-list]]))
```

If the second label is missing, the test reads `alerts-popover` as a form group of the first example. It uses no name
that `notifications-popover` defines, so the test fails and asks for a label.

### How the test works

`docs_examples_test.clj` does these steps:

1. **Read the pages.** Regular expressions find the headings, the key lines, the `**Left alone:**` lines and the fenced
   `clojure` and `edn` blocks.
2. **Split each block into examples.** `labeled-examples` parses the block with rewrite-clj. `example-items` gives each
   top-level form with its first and last row, each label and each `;; config` line. `add-example-item` reads these
   items in order and starts a new example as the Decision says.
3. **Check the form groups.** `unrelated-parts` splits an example at the blank lines between its forms, and collects the
   names that `def`-like forms define. Each form group after the first must use one of the names from above.
4. **Run each example.** `findings` writes the example to a temporary project as one file. When the example has no `ns`
   form, the test adds one with the aliases in `default-requires`, such as `rf`, `s` and `ui`. It runs the groups with
   `docs-config` and the `;; config` lines of the block. In a rule section, it counts the findings of that rule. In
   other sections, it counts the findings of every rule of the page's group, or of the groups that read Hiccup for
   `docs/rules.md`.

The reasons for these choices:

- **Regular expressions for the Markdown.** The test needs only a few line shapes. Prettier formats the pages, so these
  lines always have the same shape, and the test needs no Markdown library.
- **A parser for the Clojure.** A blank line inside a form, such as a paragraph break in a docstring, must not end an
  example. The parser gives the rows of whole forms, so only a blank line between forms counts.
- **One file for each example.** An example must not depend on another example. When each example is a file of its own,
  a name that one example defines is not visible in the next.
- **Default aliases and config.** An example stays short when it does not need its own `ns` form. Some rules do nothing
  until a config sets them, so `docs-config` and the `;; config` lines give them the values that the examples use.
- **The check on form groups.** The layout cannot show where an example with no label starts, because a blank line in a
  labeled example is a choice of layout. A form group that uses no name from above has no reason to be in the same
  example.

### When the test fails

| Message                                                         | Fix                                                                                                                                                                    |
| --------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `flagged example is not flagged`                                | Change the example so that the rule flags it, or change its label.                                                                                                     |
| `left-alone example is flagged` or `fix or left-alone …`        | Change the example, or the rule. The example can also need a `;; config` line.                                                                                         |
| `example spans a blank line, but a part uses nothing …`         | Put a label above the form group, or remove the blank line if the forms are one case.                                                                                  |
| `example has no label that starts with flagged or left alone`   | Start the first label of the block with `flagged`, `left alone` or `fix`.                                                                                              |
| `config block fails the config checks` or `config line fails …` | Use a group name, such as `:a11y`, or a top-level key, such as `:baseline`. Make `:fail-on` name tiers or rules. Read the message for a group that refuses the config. |
| `no group runs the examples of …`                               | Add the page to `page-groups` in the test.                                                                                                                             |
| `every rule shows code it flags`                                | Give the rule section a first block with a `;; flagged` part.                                                                                                          |
| `every rule the group declares has a section …`                 | Add a section with a key line for the rule, or fix the rule key in the key line.                                                                                       |
| `a link in the key line points at no heading`                   | Point the link at a heading of the same page.                                                                                                                          |
| `every page is named after a group`                             | Name the page after a group, such as `a11y.md`.                                                                                                                        |
| `code block holds no example`                                   | Add an example, or make the block a config.                                                                                                                            |
| `… does not parse`                                              | Fix the code. The message shows what the run wrote to stderr.                                                                                                          |

## Options

### Check no examples (rejected)

Rejected because the examples drift from the tool, and nothing shows it.

### Check only the first block and the Left alone block (rejected)

- Good, because these blocks have a fixed place, so the test needs no labels to read them.
- Bad, because the other blocks drift. Before this decision, an example in `a11y.md` used `ui/resize-handle` with a
  config for `my.ui/resize-handle`, so it did not show what its label said.

### A blank line ends an example (rejected)

- Good, because it needs no label.
- Bad, because an example of two forms cannot have a blank line between them, so the code does not look like real code.
- Bad, because a label comment between two forms splits the example, and the test checks each form alone.

### A `;; ---` line ends an example (rejected)

- Good, because the end of each example is clear.
- Bad, because every page shows a separator line that means nothing to a reader.
- Bad, because a missing separator joins two examples, and nothing finds it.

### A Markdown parser to read the pages (rejected)

- Good, because it reads every Markdown shape, such as a `~~~` fence or a `#` line in a `bash` block.
- Bad, because it adds a test dependency.
- Bad, because the test must still find the GitHub anchor of each heading with its own code.

### A label starts an example, and every block runs (chosen)

- Good, because the pages already label most examples with a comment.
- Good, because blank lines can stay between the forms of one example.
- Good, because no code block can drift: a block that the test cannot read fails.
- Good, because the check on form groups finds an example with no label that joins the example above it.
- Bad, because an example of several forms must have a label.
- Bad, because a form group that uses no name from above, such as a second `deftest` on the same `def`, needs its own
  label.

## Consequences

- CI runs the test. `.github/workflows/test.yml` runs `clojure -M:test` on each pull request and each push to `main`, so
  a pull request with a wrong example shows a failed check. `.github/workflows/release.yml` runs the test on each `v*`
  tag, and the release jobs need it, so a release does not build while an example is wrong.
- A pull request that changes a rule must also change each example that the change makes false.
- A change to the layout of the docs can need a change to the test. A change to the prose, or to an example that follows
  the rules of this record, needs no change to the test.
- In a rule section, an example is checked only against its own rule. A fix can still give a finding of another rule.

## Confirmation

- `clojure -M:test -n cljs-patrol.docs-examples-test` passes.
- The test reads every fenced `clojure` and `edn` block of `docs/rules.md`, `docs/rules/*.md` and `README.md`.

## Revisit when

- An example needs two form groups, and neither form group uses a name that the other defines.
- A regular expression reads a page incorrectly, or a page needs a Markdown shape that the regular expressions cannot
  read. Then use a Markdown parser for step 1.
- A Markdown file outside `docs/adr/` gets a `clojure` or `edn` block, such as a guide outside `docs/rules/`.

## More Information

- The test: [`test/cljs_patrol/docs_examples_test.clj`](../../test/cljs_patrol/docs_examples_test.clj)
- The pull request that made every block a test:
  [olecve/cljs-patrol#156](https://github.com/olecve/cljs-patrol/pull/156)
