# cljs-patrol

Static analysis tool for ClojureScript UI codebases. Currently detects unused and phantom re-frame subscriptions and events.

## Usage

```bash
clojure -M:run <source-dir> [<source-dir> ...]
```

Example:

```bash
clojure -M:run src/cljs/myapp
```

Exits with code `1` when issues are found, making it suitable for CI pipelines.

## What it detects

- **Unused subscriptions** — registered with `reg-sub` but never subscribed to
- **Unused events** — registered with `reg-event-*` but never dispatched
- **Phantom subscriptions** — subscribed to but never declared
- **Phantom events** — dispatched but never declared
- **Dynamic dispatch/subscribe sites** — dispatch or subscribe calls with a non-literal keyword (manual review needed)

## Supported patterns

Declarations: `reg-sub`, `reg-event-db`, `reg-event-fx`, `reg-event-ctx`, `reg-fx`, `reg-cofx`

Usages: `subscribe`, `dispatch`, `dispatch-sync`, `:<-` signal inputs, `:fx` vector tuples
(`:dispatch`, `:dispatch-n`, `:dispatch-later`), `:on-success` / `:on-failure` / `:on-error` http callbacks

## Build

Build a standalone uberjar:

```bash
clojure -T:build uber
java -jar target/cljs-patrol-0.1.0.jar <source-dir>
```

## Formatting

```bash
clojure -M:cljstyle fix src/
```
