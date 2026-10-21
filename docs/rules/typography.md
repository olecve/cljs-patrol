# Typography rules

The rules of the `typography` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Run this group alone with `--only typography`.

## Rules

### Mixed typography token groups

`mixed-token-groups` · deprecations — Typography tokens from two Figma token groups in one style.

```clojure
;; flagged
(defclass label-style []
  {:font system-theme/action-small-font
   :letter-spacing system-theme/action-large-letter-spacing})

;; fix
(defclass label-style []
  {:font system-theme/action-small-font
   :letter-spacing system-theme/action-small-letter-spacing})
```

<details>
<summary>Details</summary>

A style declaration (`defclass` or `defattrs`) whose typography tokens come from more than one Figma token group. Each
declaration takes its tokens from a single group, the one prefix they share: `action-small-font` +
`action-small-letter-spacing` + `action-small-text-case`. The available groups:

- `caption-{small,medium,large,xlarge}`
- `body-short-{small,medium,large}`
- `body-long-{small,medium,large}`
- `subhead-{small,medium,large}`
- `heading-reg-{small,medium,large,xlarge}`
- `heading-bold-{small,medium,large,xlarge}`
- `action-{small,medium,large}`
- `code-compact-{small,medium,large}`
- `code-reg-{small,medium,large}`

</details>
