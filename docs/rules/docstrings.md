# Docstrings rules

The rules of the `docstrings` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

- **Docstring summary** (`docstring-summary`, cleanup) — first line of a multi-line docstring is not a self-contained
  sentence ending in `.`, `!`, `?`, or `:`
- **Docstring indentation** (`docstring-indentation`, cleanup) — continuation lines of a multi-line docstring are
  indented less than the opening-quote column
- **Docstring leading/trailing whitespace** (`docstring-leading-trailing-whitespace`, cleanup) — docstring starts or
  ends with whitespace
