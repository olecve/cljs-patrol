# ADR 0004: Run The Rule Docs Examples As Tests

## Status

Accepted, 2026-11-16

## Context

Each rule has a section in `docs/rules/`. The section shows code that the rule flags, the fixed code, and code that the
rule does not flag. A reader learns a rule from these examples, so an example that is wrong teaches the wrong rule.

Prose and code examples drift from the tool when nothing checks them. A rule change can make an example false, and the
change does not touch the docs page. A new rule can also ship with no section.

Pull request #90 (2026-10-19) made the examples tests. `test/cljs_patrol/docs_examples_test.clj` reads every page and
runs each example through the rule that the section names:

- A flagged example must give a finding of that rule.
- A fixed or left-alone example must give no finding of that rule.
- Every example must parse.
- Every rule that a group declares must have a section, and every link in a key line must point to a heading.

On 2026-11-16 the pages had 67 rule sections with 86 flagged, 79 fixed and 107 left-alone examples.

To run an example alone, the test must find where one example ends and the next starts. Pull request #90 ended an
example at a blank line between two top-level forms. This rule had two problems:

- An example of two forms, such as a component and the button that renders it, could not have a blank line between the
  forms. A blank line split the pair, and the test then linted each half as a file of its own.
- A label comment between two forms also split the example. For example, in the left-alone part of `reg-fx-public-fn`,
  the test linted `(rf/reg-fx ::show-error show-error!)` without the `defn-` that it registers. The example did not
  check what its label said.

## Decision

The rule docs examples are tests, and a comment starts each example.

- **The examples run.** Every flagged, fixed and left-alone example runs through its rule as a file of its own, and the
  build fails when the result is not the expected result. A change to a rule thus updates its docs in the same pull
  request.
- **A comment starts an example.** A `;;` comment at the start of a line starts a new example. The example continues to
  the next such comment, and blank lines in it stay in it. The `;; flagged` and `;; fix` markers count as such comments.
- **An example with no comment ends at a blank line.** This keeps the old behavior for a part with no labels.
- **A part must use a name from above.** In an example that has a blank line between top-level forms, each part after
  the blank line must use a name that an earlier part defines. If it does not, the part is a different example with no
  label, and the test fails. The fix is to give that part its own comment.
- **The layout follows the code style.** A label goes above the first form of an example, not between its forms. A form
  that uses a name that a form above it defines has a blank line before it.

### How the test works

`docs_examples_test.clj` does these steps for each page:

1. **Find the sections.** `rule-sections` reads the Markdown with regular expressions. It splits the page at its
   headings, and it takes the code out of the ` ```clojure ` blocks. A section is a rule section when it has a key line,
   such as `` `reg-fx-public-fn` · cleanup — … ``. The first code block of the section holds the `;; flagged` part and
   the `;; fix` part. The code block after `**Left alone:**` holds the left-alone part.
2. **Split each part into examples.** `chunks` parses the part with rewrite-clj. `example-items` gives each top-level
   form with its first and last row, and each comment that starts at column 1. `add-example-item` reads these items in
   order. A comment starts a new example. A form after a blank line starts a new example only when the current example
   has no comment. Any other form joins the current example. The test puts `;; flagged` or `;; fix` back in front of its
   part, so that these parts start with a comment too.
3. **Check the parts of each example.** `unrelated-parts` splits an example at the blank lines between its top-level
   forms. It collects the names that `def`-like forms define (`def`, `defn`, `defn-`, `defclass`, `defattrs` and the
   others whose head starts with `def`). Each part after the first must contain a symbol with one of the names from the
   parts above it.
4. **Run each example.** `findings` writes the example to a temporary project as one file. When the example has no `ns`
   form, the test adds one with the aliases in `default-requires`, such as `rf` and `s`. It runs the rule group with
   `docs-config` and counts the findings of the rule of the section.

The reasons for these choices:

- **Regular expressions for the Markdown.** The test needs only the headings, the key line, the `**Left alone:**` line,
  the `clojure` code blocks and the links. Prettier formats the pages, so these lines always have the same shape. A
  small set of regular expressions reads them, and the test needs no Markdown library.
- **A parser for the Clojure, not a line split.** A blank line inside a form, such as a paragraph break in a docstring,
  must not end an example. The parser gives the rows of whole forms, so only a blank line between forms counts.
- **One file for each example.** An example must not depend on another example. When each example is a file of its own,
  a name that one example defines is not visible in the next.
- **Default aliases and config.** An example stays short when it does not need its own `ns` form. Some rules do nothing
  until a config sets them, so `docs-config` gives them the values that the examples use.
- **The check on later parts.** A blank line in a commented example is a choice of layout, so the test cannot see from
  the layout where an example with no label starts. A part that uses no name from above has no reason to be in the same
  example. The check is simple, and it found all 20 joined examples in pull request #155.

## Options

### Prose docs with no check (rejected)

Rejected because the docs drift from the tool, and nothing shows it. Pull request #90 replaced this.

### A blank line ends an example (rejected)

- Good, because it needs no label.
- Bad, because an example of two forms cannot have a blank line between them, so the code does not look like real code.
- Bad, because a label comment between two forms splits the example, and the test checks each half alone.

This was the rule from pull request #90 to pull request #155.

### A `;; ---` line ends an example (rejected)

- Good, because the end of each example is clear.
- Bad, because every page shows a separator line that means nothing to a reader.
- Bad, because a missing separator joins two examples, and nothing finds it.

### A Markdown parser to read the pages (rejected)

- Good, because it reads every Markdown shape, such as a `~~~` fence or a `#` line in a `bash` block.
- Bad, because it adds a test dependency, and the code that walks its syntax tree is not shorter than the regular
  expressions.
- Bad, because the test must still find the GitHub anchor of each heading with its own code.

On 2026-11-16 no code block in the pages had a line that the heading expression reads as a heading.

### A comment starts an example (chosen)

- Good, because the pages already label most examples with a comment, so most pages need no change.
- Good, because blank lines can stay between the forms of one example.
- Good, because the check on later parts finds an example with no label that joins the example above it.
- Bad, because an example of several forms must have a label.
- Bad, because a part that uses no name from above, such as a second `deftest` on the same `def`, needs its own label.

## Consequences

- CI runs the test. `.github/workflows/test.yml` runs `clojure -M:test` on each pull request and each push to `main`,
  and `.github/workflows/release.yml` runs it on each `v*` tag before the build. Thus no pull request merges and no
  release ships while an example disagrees with the tool.
- The docs cannot show behavior that the tool does not have. A pull request that changes a rule fails until its docs
  agree.
- A change to the layout of the docs can need a change to the test, as in pull request #155. A change to the prose or to
  an example that follows the layout rules needs no change to the test.
- Pull request #155 added labels to 20 flagged and fixed examples, and moved 5 label comments above the first form of
  their example. In those 5 examples the old split had checked each form alone.
- Each fix is checked only against its own rule. A fix can still give a finding of another rule.

## Revisit when

- An example must show forms that use no names of each other, and a label for each part does not read well.
- A page needs code blocks that are not examples, such as config, inside a rule section.
- A docs format other than Markdown code blocks replaces the pages, for example a generated site.
- A regular expression reads a page incorrectly, or a page needs a Markdown shape that the regular expressions cannot
  read. Then use a Markdown parser for step 1.

## More Information

- Pull request #90, which made the examples tests:
  [olecve/cljs-patrol#90](https://github.com/olecve/cljs-patrol/pull/90)
- Pull request #155, which starts each example at a comment:
  [olecve/cljs-patrol#155](https://github.com/olecve/cljs-patrol/pull/155)
- The test:
  [`test/cljs_patrol/docs_examples_test.clj` at 7277400](https://github.com/olecve/cljs-patrol/blob/7277400/test/cljs_patrol/docs_examples_test.clj)
