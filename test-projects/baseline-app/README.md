# baseline-app

Fixture project for testing cljs-patrol's baseline feature.

Contains a minimal ClojureScript app with a known, stable set of issues:

- **Unused subscription** - `::subs/old-dashboard` registered but never subscribed to
- **Unused event** - `::events/legacy-reset` registered but never dispatched
- **Phantom subscription** - `::subs/deleted-feature` subscribed to but never declared
- **Unused style** - `styles/legacy-panel` declared but never called
- **defclass as sole attr** - `styles/container` used only as `{:class (styles/container)}`
- **Deprecated effect** - `::events/batch-notify` uses `:dispatch-n`
- **reg-sub `:=>` with 1-arity fn** - `::subs/latest-active-user` uses `:=> last`, should be `:->`
- **reg-event-fx returns only :db** - `::events/fetch-data` should be `reg-event-db`
- **reg-event-fx empty effects** - `::events/no-op` returns `{}`, the handler does nothing
- **reg-event-db clobbers db** - `::events/legacy-reset` returns `{}`, replacing the entire app-db

These issues are exercised by `cljs_patrol.baseline_integration_test` to verify the full baseline workflow: writing a baseline, reading it back, diffing against re-analysis, console/EDN output formatting, quiet and strict modes, and round-trip stability.
