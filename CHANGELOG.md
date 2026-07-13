# Changelog

## [v0.0.15] - 2026-07-13

## What's Changed

* Stack rule descriptions under the title and linkify URLs in HTML report by @olecve in https://github.com/olecve/cljs-patrol/pull/35
* Add Expand all / Collapse all buttons to the HTML report by @olecve in https://github.com/olecve/cljs-patrol/pull/36
* Content-based identity for Hiccup baseline entries by @olecve in https://github.com/olecve/cljs-patrol/pull/37

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.14...v0.0.15


## [v0.0.14] - 2026-07-10

## What's Changed

* Bake cljs-patrol version into a resource so baselines record real version by @olecve in https://github.com/olecve/cljs-patrol/pull/32
* Refresh README for rules added in v0.0.13 by @olecve in https://github.com/olecve/cljs-patrol/pull/33
* Extend :missing-accessible-name to dialog / drawer shapes by @olecve in https://github.com/olecve/cljs-patrol/pull/34

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.13...v0.0.14


## [v0.0.13] - 2026-07-10

## What's Changed

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

## What's Changed

* Include source snippet in a11y findings by @olecve in https://github.com/olecve/cljs-patrol/pull/23

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.11...v0.0.12


## [v0.0.11] - 2026-07-07

## What's Changed

* Add a11y rule group with :img-alt-missing by @olecve in https://github.com/olecve/cljs-patrol/pull/20
* Extract shared Hiccup helpers into cljs-patrol.hiccup by @olecve in https://github.com/olecve/cljs-patrol/pull/21
* Add :invalid-tabindex rule to a11y group by @olecve in https://github.com/olecve/cljs-patrol/pull/22

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.10...v0.0.11


## [v0.0.10] - 2026-06-16

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.9...v0.0.10


## [v0.0.9] - 2026-06-10

## What's Changed

* Detect reg-event-db returning an effects-style map by @olecve in https://github.com/olecve/cljs-patrol/pull/19

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.8...v0.0.9


## [v0.0.8] - 2026-06-10

## What's Changed

* Add docstrings rule group by @olecve in https://github.com/olecve/cljs-patrol/pull/18

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.7...v0.0.8


## [v0.0.7] - 2026-05-19

## What's Changed

* Fix release workflow: fetch main before checkout by @olecve in https://github.com/olecve/cljs-patrol/pull/12
* Add --list-rules flag by @olecve in https://github.com/olecve/cljs-patrol/pull/13
* Unify blocking/warning count helpers in severity ns by @olecve in https://github.com/olecve/cljs-patrol/pull/14
* Detect reg-sub :=> with 1-arity fn by @olecve in https://github.com/olecve/cljs-patrol/pull/15
* Detect reg-event-fx with empty effects or only :db returned by @olecve in https://github.com/olecve/cljs-patrol/pull/16

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.6...v0.0.7

## [v0.0.6] - 2026-05-19

## What's Changed

* Add severity tiers and --fail-on flag by @olecve in https://github.com/olecve/cljs-patrol/pull/8
* Remove leftover cljstyle config by @olecve in https://github.com/olecve/cljs-patrol/pull/9
* Tidy docstrings and inline short defn signatures by @olecve in https://github.com/olecve/cljs-patrol/pull/10
* Extract HTML report CSS to its own resource file by @olecve in https://github.com/olecve/cljs-patrol/pull/11

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.5...v0.0.6

## [v0.0.5] - 2026-04-30

## What's Changed

* Switch formatting tool from cljstyle to cljfmt by @olecve in https://github.com/olecve/cljs-patrol/pull/6
* Add baseline support for incremental adoption by @olecve in https://github.com/olecve/cljs-patrol/pull/7

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.4...v0.0.5

## [v0.0.4] - 2026-03-18

## What's Changed

* Fix reagent group to reuse spade parse handlers by @olecve in https://github.com/olecve/cljs-patrol/pull/5

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.3...v0.0.4

## [v0.0.3] - 2026-03-18

## What's Changed

* Add defclass/defattrs usage rules by @olecve in https://github.com/olecve/cljs-patrol/pull/3
* Add :class vector detection and reagent rule group by @olecve in https://github.com/olecve/cljs-patrol/pull/4

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.2...v0.0.3

## [v0.0.2] - 2026-03-17

## What's Changed

* Add markdown output format by @olecve in https://github.com/olecve/cljs-patrol/pull/2

**Full Changelog**: https://github.com/olecve/cljs-patrol/compare/v0.0.1...v0.0.2

## [v0.0.1] - 2026-03-17

## What's Changed

* Decouple reporter logic from rule groups by @olecve in https://github.com/olecve/cljs-patrol/pull/1

## New Contributors

* @olecve made their first contribution in https://github.com/olecve/cljs-patrol/pull/1

**Full Changelog**: https://github.com/olecve/cljs-patrol/commits/v0.0.1

