# ADR 0015: Replace Prefer Tab Index With Forbid Props Config

## Status

Accepted, 2026-11-26 (proposed 2026-11-26)

## Context

cljs-patrol checks ClojureScript source without executing it. Reagent writes native elements and components as Hiccup
vectors. Their prop names can follow different conventions. Native DOM props can use `:tab-index`, while a React
component can require `:tabIndex`.

[CP-0070](../../backlog/tasks/cp-0070%20-%20Prefer-tab-index-in-Reagent-DOM-Hiccup.md) added the dedicated
`prefer-tab-index` rule. It reports `:tabIndex` and `:tabindex` on native elements by default. It reads props through
`assoc`, `merge` and bindings. Review rounds kept finding bugs in its handling of tags and expression positions.

The last release is `v0.0.63`. The dedicated rule was never released. Its removal therefore breaks no released rule
contract. This record replaces the CP-0070 implementation decision.

The existing `forbid-props` group reports configured prop names and gives a configured message. It does nothing until
configured. Its entries can name individual tags and components, but cannot select all native tags without a list.

A spelling policy can apply to other props, such as `:className` or `:htmlFor`. Separate rules would repeat the same
selection and reporting code.

## Decision

Replace `prefer-tab-index` with `forbid-props` configuration. Add `:disallowed-for :native` to select native HTML and
SVG tags without selecting components. Keep the element matching of existing entries unchanged. The new target uses the
same element reader.

Configure separate entries for `:tabIndex` and `:tabindex`, each with the message `Use :tab-index.`. Remove the
dedicated rule, its registration, tier entry, baseline identity, tests and rule documentation. Keep helpers that other
rules use.

## Options

### Keep the dedicated rule (rejected)

- Good, because it reports the spelling by default without configuration.
- Good, because it reads props through `assoc`, `merge` and bindings, including some partially known maps.
- Bad, because it adds code for one spelling policy that the generic group can express.
- Bad, because review rounds kept finding bugs in that code. Its separate expression checks increase maintenance work.

### Configure `forbid-props` with a native target (chosen)

- Good, because one generic target covers native HTML and SVG tags without a tag list in each configuration.
- Good, because the same target supports other spelling policies, such as `:className` or `:htmlFor`.
- Good, because a configured message tells the user which prop name to use.
- Bad, because the group reads literal prop maps only. It can resolve bindings and calls only when their map is fully
  known.
- Bad, because the group does nothing until configured. Removing the default rule reduces automatic coverage.
- Bad, because ambiguous data vectors still need the shared Hiccup checks. The target does not remove those limits.

### Configure `forbid-props` with every tag listed (rejected)

- Good, because the existing collection target needs no code change.
- Bad, because each configuration needs a long list of HTML and SVG tags.
- Bad, because a missing tag silently reduces coverage. Each new tag requires another configuration change.

## Consequences

The spelling policy uses the existing `forbidden-prop` rule and its `deprecations` tier. It does not retain the removed
rule's `cleanup` tier. Findings use the existing prop identity and configured message.

Components, adapted React components and `[:> Comp ...]` interop remain outside the native target. The target also
excludes fragments. The shared tag recognition limits still apply.

A `:native` entry also reports props inside a `(comment …)` form, as every `forbid-props` entry does. Add these findings
to the baseline or remove the forbidden props.

The `invalid-tabindex` accessibility rule continues to check values. It reads `:tab-index` and `:tabIndex`. The
configuration changes spelling policy only. It does not replace value checks.

## Confirmation

Before acceptance, confirm these results:

- Native HTML and SVG examples report both forbidden spellings with the configured message.
- Components, adapted React components and React interop examples produce no finding from the native target.
- A malformed target stops the run with a configuration error.
- Rule documentation examples execute as tests, as [ADR 0004](0004-rule-docs-examples-are-tests.md) requires.
- The full test suite, formatting checks, CI lint checks, file access check and own-source patrol pass.
- The scoped mutation run for `forbid-props` catches each scored mutant in the changed code.

## Revisit when

- A required spelling check must report props from a map that `forbid-props` cannot fully read.
- A supported native tag fails to match the target in an executable regression test.
- A proposed default spelling policy requires coverage without configuration.

## More Information

- Original implementation: [PR #189](https://github.com/olecve/cljs-patrol/pull/189).
- Current configuration and limits: [Forbid props options](../rules/forbid-props.md#forbid-props-options).
- Target implementation: [`forbid_props.clj`](../../src/cljs_patrol/groups/forbid_props.clj).
- Independent value checks: [Invalid tabindex](../rules/a11y.md#invalid-tabindex).
- The pull request: [olecve/cljs-patrol#190](https://github.com/olecve/cljs-patrol/pull/190)
