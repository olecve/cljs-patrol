# Changelog

## [v0.0.34] - 2026-10-05

* Flag a ^js type hint written in test code by @olecve in https://github.com/olecve/cljs-patrol/pull/80
* Read .clj files behind --experimental-clj by @olecve in https://github.com/olecve/cljs-patrol/pull/81
* Move the fixture projects out of the test classpath root by @olecve in https://github.com/olecve/cljs-patrol/pull/82
* Cover props-slot, and stop writing the body walk three times by @olecve in https://github.com/olecve/cljs-patrol/pull/83

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.33...v0.0.34

## [v0.0.33] - 2026-10-01

* Guard a rule declared in either place, not only one by @olecve in https://github.com/olecve/cljs-patrol/pull/77
* Normalize a form the same way everywhere, brackets included by @olecve in https://github.com/olecve/cljs-patrol/pull/79
* Flag a test reading a private var through its var quote by @olecve in https://github.com/olecve/cljs-patrol/pull/78

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.32...v0.0.33

## [v0.0.32] - 2026-10-01

* Give every rule a baseline identity, and fail the build when one lacks it by @olecve in https://github.com/olecve/cljs-patrol/pull/76

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.31...v0.0.32

## [v0.0.31] - 2026-09-30

* Flag a test var whose name opens with an article by @olecve in https://github.com/olecve/cljs-patrol/pull/73
* Flag an assertion message sharing a line with its expression by @olecve in https://github.com/olecve/cljs-patrol/pull/74
* Flag a test that decides what to assert while it runs by @olecve in https://github.com/olecve/cljs-patrol/pull/75

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.30...v0.0.31

## [v0.0.30] - 2026-09-30

* Flag a label that labels nothing by @olecve in https://github.com/olecve/cljs-patrol/pull/72

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.29...v0.0.30

## [v0.0.29] - 2026-09-28

* Stop the focus rules reporting the ways out of them they cannot read by @olecve in https://github.com/olecve/cljs-patrol/pull/62
* Read a caption candidate's head safely so an empty child cannot end the run by @olecve in https://github.com/olecve/cljs-patrol/pull/63
* Fix the shared helpers the focus rules were working around by @olecve in https://github.com/olecve/cljs-patrol/pull/64
* Keep the line a finding sits on out of what identifies it by @olecve in https://github.com/olecve/cljs-patrol/pull/66
* Tell props from body by what a call yields, and stop following what cannot take focus by @olecve in https://github.com/olecve/cljs-patrol/pull/68
* Ask a name of tablist, menu and menubar, and say that the spec does not by @olecve in https://github.com/olecve/cljs-patrol/pull/65
* Wrap the markdown at 120 columns with prettier by @olecve in https://github.com/olecve/cljs-patrol/pull/69
* Split the README into reference pages under docs/, and merge the duplicate config section by @olecve in https://github.com/olecve/cljs-patrol/pull/70
* Drop the docstrings that only restate the name, and let summaries stand out by @olecve in https://github.com/olecve/cljs-patrol/pull/71

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.28...v0.0.29

## [v0.0.28] - 2026-09-28

* Flag aria-hidden on focusable elements, nested controls, and unnamed container roles by @olecve in https://github.com/olecve/cljs-patrol/pull/61

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.27...v0.0.28

## [v0.0.27] - 2026-09-23

* Flag a redundant into around hiccup only when the mapped body keys its elements by @olecve in https://github.com/olecve/cljs-patrol/pull/60

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.26...v0.0.27

## [v0.0.26] - 2026-09-22

* Flag Spade selectors that Garden compiles into CSS matching nothing by @olecve in https://github.com/olecve/cljs-patrol/pull/57
* Flag Spade style maps written out of CSS property order by @olecve in https://github.com/olecve/cljs-patrol/pull/58
* Read a style declaration whole: through its metadata and into its nesting by @olecve in https://github.com/olecve/cljs-patrol/pull/59

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.25...v0.0.26

* Flag Spade selectors that Garden compiles into CSS matching nothing by @olecve in https://github.com/olecve/cljs-patrol/pull/57
* Flag Spade style maps written out of CSS property order by @olecve in https://github.com/olecve/cljs-patrol/pull/58
* Read a style declaration whole: through its metadata and into its nesting by @olecve in https://github.com/olecve/cljs-patrol/pull/59

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.25...v0.0.26

## [v0.0.26] - 2026-09-22

* Flag Spade selectors that Garden compiles into CSS matching nothing by @olecve in https://github.com/olecve/cljs-patrol/pull/57
* Flag Spade style maps written out of CSS property order by @olecve in https://github.com/olecve/cljs-patrol/pull/58
* Read a style declaration whole: through its metadata and into its nesting by @olecve in https://github.com/olecve/cljs-patrol/pull/59

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.25...v0.0.26

## [v0.0.25] - 2026-09-18

* Resolve :aria-label through let-bound map literals so shared props helpers stop tripping missing-accessible-name by @olecve in https://github.com/olecve/cljs-patrol/pull/55
* Read a built map as complete when every part of it is readable by @olecve in https://github.com/olecve/cljs-patrol/pull/56

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.24...v0.0.25

## [v0.0.24] - 2026-09-14

* Run CI on every branch push, not only on main and pull requests by @olecve in https://github.com/olecve/cljs-patrol/pull/53
* Flag a literal accessible name repeated by every item of a list by @olecve in https://github.com/olecve/cljs-patrol/pull/54

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.23...v0.0.24

## [v0.0.23] - 2026-09-08

* Require an accessible name on role=img, not just dialog by @olecve in https://github.com/olecve/cljs-patrol/pull/52

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.22...v0.0.23

## [v0.0.22] - 2026-09-07

* See :on-click through assoc, merge and assoc-in attr wrappers by @olecve in https://github.com/olecve/cljs-patrol/pull/51

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.21...v0.0.22

## [v0.0.21] - 2026-09-07

* Add a11y rule: aria-live-contradicts-role by @olecve in https://github.com/olecve/cljs-patrol/pull/46
* Add clj-kondo, with the linters it ships switched off turned on by @olecve in https://github.com/olecve/cljs-patrol/pull/47
* Explain the rule in console output, not only in html and markdown by @olecve in https://github.com/olecve/cljs-patrol/pull/48
* Catch icon-as-clickable and icon-only unnamed buttons by @olecve in https://github.com/olecve/cljs-patrol/pull/49
* Document svg, namespace-glob aliases, and icon-only button bodies in the README by @olecve in https://github.com/olecve/cljs-patrol/pull/50

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.20...v0.0.21

## [v0.0.20] - 2026-09-01

* Fix --baseline-write crashing in native builds by @olecve in https://github.com/olecve/cljs-patrol/pull/44
* Write the baseline atomically so a failed write cannot destroy it by @olecve in https://github.com/olecve/cljs-patrol/pull/45

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.19...v0.0.20

## [v0.0.19] - 2026-07-23

* Add :summary block to baseline for PR-diff friendly counts by @olecve in https://github.com/olecve/cljs-patrol/pull/43

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.18...v0.0.19

## [v0.0.18] - 2026-07-17

* Extract filesystem helpers into cljs-patrol.fs by @olecve in https://github.com/olecve/cljs-patrol/pull/41
* Add :redundant-into-hiccup rule to the reagent group by @olecve in https://github.com/olecve/cljs-patrol/pull/42

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.17...v0.0.18

## [v0.0.17] - 2026-07-13

* Keep baseline lines under 120 columns by @olecve in https://github.com/olecve/cljs-patrol/pull/39

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.16...v0.0.17

## [v0.0.16] - 2026-07-13

* Sort baseline entries by rule then file then form by @olecve in https://github.com/olecve/cljs-patrol/pull/38

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.15...v0.0.16

## [v0.0.15] - 2026-07-13

* Stack rule descriptions under the title and linkify URLs in HTML report by @olecve in https://github.com/olecve/cljs-patrol/pull/35
* Add Expand all / Collapse all buttons to the HTML report by @olecve in https://github.com/olecve/cljs-patrol/pull/36
* Content-based identity for Hiccup baseline entries by @olecve in https://github.com/olecve/cljs-patrol/pull/37

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.14...v0.0.15

## [v0.0.14] - 2026-07-10

* Bake cljs-patrol version into a resource so baselines record real version by @olecve in https://github.com/olecve/cljs-patrol/pull/32
* Refresh README for rules added in v0.0.13 by @olecve in https://github.com/olecve/cljs-patrol/pull/33
* Extend :missing-accessible-name to dialog / drawer shapes by @olecve in https://github.com/olecve/cljs-patrol/pull/34

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.13...v0.0.14

## [v0.0.13] - 2026-07-10

* Add :onclick-on-non-interactive rule by @olecve in https://github.com/olecve/cljs-patrol/pull/24
* Fix :.class/:#id shorthand + reconcile onclick suggestion by @olecve in https://github.com/olecve/cljs-patrol/pull/25
* Value-aware :role/handler checks + pointer events by @olecve in https://github.com/olecve/cljs-patrol/pull/26
* Add :empty-interactive-element rule + Spade-context skip by @olecve in https://github.com/olecve/cljs-patrol/pull/27
* Widen :empty-interactive-element (role= and icon-only) by @olecve in https://github.com/olecve/cljs-patrol/pull/28
* Detect misplaced pseudo-selectors in Spade main style map by @olecve in https://github.com/olecve/cljs-patrol/pull/29
* Detect consecutive self-selectors in Spade sibling vectors by @olecve in https://github.com/olecve/cljs-patrol/pull/30
* Add :missing-accessible-name a11y rule with config-driven component aliases by @olecve in https://github.com/olecve/cljs-patrol/pull/31

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.12...v0.0.13

## [v0.0.12] - 2026-07-07

* Include source snippet in a11y findings by @olecve in https://github.com/olecve/cljs-patrol/pull/23

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.11...v0.0.12

## [v0.0.11] - 2026-07-07

* Add a11y rule group with :img-alt-missing by @olecve in https://github.com/olecve/cljs-patrol/pull/20
* Extract shared Hiccup helpers into cljs-patrol.hiccup by @olecve in https://github.com/olecve/cljs-patrol/pull/21
* Add :invalid-tabindex rule to a11y group by @olecve in https://github.com/olecve/cljs-patrol/pull/22

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.10...v0.0.11

## [v0.0.10] - 2026-06-16

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.9...v0.0.10

## [v0.0.9] - 2026-06-10

* Detect reg-event-db returning an effects-style map by @olecve in https://github.com/olecve/cljs-patrol/pull/19

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.8...v0.0.9

## [v0.0.8] - 2026-06-10

* Add docstrings rule group by @olecve in https://github.com/olecve/cljs-patrol/pull/18

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.7...v0.0.8

## [v0.0.7] - 2026-05-19

* Fix release workflow: fetch main before checkout by @olecve in https://github.com/olecve/cljs-patrol/pull/12
* Add --list-rules flag by @olecve in https://github.com/olecve/cljs-patrol/pull/13
* Unify blocking/warning count helpers in severity ns by @olecve in https://github.com/olecve/cljs-patrol/pull/14
* Detect reg-sub :=> with 1-arity fn by @olecve in https://github.com/olecve/cljs-patrol/pull/15
* Detect reg-event-fx with empty effects or only :db returned by @olecve in https://github.com/olecve/cljs-patrol/pull/16

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.6...v0.0.7

## [v0.0.6] - 2026-05-19

* Add severity tiers and --fail-on flag by @olecve in https://github.com/olecve/cljs-patrol/pull/8
* Remove leftover cljstyle config by @olecve in https://github.com/olecve/cljs-patrol/pull/9
* Tidy docstrings and inline short defn signatures by @olecve in https://github.com/olecve/cljs-patrol/pull/10
* Extract HTML report CSS to its own resource file by @olecve in https://github.com/olecve/cljs-patrol/pull/11

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.5...v0.0.6

## [v0.0.5] - 2026-04-30

* Switch formatting tool from cljstyle to cljfmt by @olecve in https://github.com/olecve/cljs-patrol/pull/6
* Add baseline support for incremental adoption by @olecve in https://github.com/olecve/cljs-patrol/pull/7

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.4...v0.0.5

## [v0.0.4] - 2026-03-18

* Fix reagent group to reuse spade parse handlers by @olecve in https://github.com/olecve/cljs-patrol/pull/5

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.3...v0.0.4

## [v0.0.3] - 2026-03-18

* Add defclass/defattrs usage rules by @olecve in https://github.com/olecve/cljs-patrol/pull/3
* Add :class vector detection and reagent rule group by @olecve in https://github.com/olecve/cljs-patrol/pull/4

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.2...v0.0.3

## [v0.0.2] - 2026-03-17

* Add markdown output format by @olecve in https://github.com/olecve/cljs-patrol/pull/2

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.1...v0.0.2

## [v0.0.1] - 2026-03-17

* Decouple reporter logic from rule groups by @olecve in https://github.com/olecve/cljs-patrol/pull/1

### New Contributors

* @olecve made their first contribution in https://github.com/olecve/cljs-patrol/pull/1

**Full Changelog**: https://github.com/olecve/cljs-patrol/commits/v0.0.1
