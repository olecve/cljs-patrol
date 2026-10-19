# Typography rules

The rules of the `typography` group: what each one flags and what it deliberately leaves alone. Each rule carries the
key that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

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

Typography tokens from different Figma token groups mixed in a single style definition

</details>
