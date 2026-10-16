# Re-frame rules

The rules of the `re-frame` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Declarations are read from `reg-sub`, `reg-event-db`, `reg-event-fx`, `reg-event-ctx`, `reg-fx` and `reg-cofx`. Usages
are read from `subscribe`, `dispatch`, `dispatch-sync`, `:<-` signal inputs, `:fx` vector tuples (`:dispatch`,
`:dispatch-n`, `:dispatch-later`), and `:on-success` / `:on-failure` / `:on-error` http callbacks.

- **Unused subscriptions** (`unused-subs`, cleanup) — registered with `reg-sub` but never subscribed to
- **Unused events** (`unused-events`, cleanup) — registered with `reg-event-*` but never dispatched
- **Phantom subscriptions** (`phantom-subs`, cleanup) — subscribed to but never declared
- **Phantom events** (`phantom-events`, cleanup) — dispatched but never declared
- **Duplicate registrations** (`duplicate-subs`, `duplicate-events`, bugs) — two `reg-sub` or `reg-event-*` calls with
  the same keyword (second silently overwrites the first)
- **reg-event-db returning effects** (`reg-event-db-returning-effects`, bugs) — `reg-event-db` handler returns an
  effects-style `{:db ... :dispatch ...}` map; the whole map silently replaces app-db and extra effects are dropped (use
  `reg-event-fx` instead)
- **reg-event-db empty** (`reg-event-db-empty`, bugs) — a `reg-event-db` handler returns `nil` or `{}`, which replaces
  the whole app-db with nothing. Return the db, changed; a deliberate reset reads better as `(assoc db …)` or a named
  initial value
- **reg-event-fx empty** (`reg-event-fx-empty`, bugs) — a `reg-event-fx` handler returns `nil` or an empty effects map,
  so it does nothing. Remove it, or return the effects it was meant to
- **reg-event-fx db only** (`reg-event-fx-db-only`, cleanup) — a `reg-event-fx` handler returns only `:db`.
  `reg-event-db` takes a handler returning the new db directly, which says the same with less
- **reg-sub :=> with a 1-arity fn** (`reg-sub-=>-1-arity`, cleanup) — `reg-sub` uses the `:=>` sugar with a one-argument
  fn. `:=>` calls it with the signal value and the query vector, so in ClojureScript the query vector is silently
  dropped. Use `:->`, which passes the signal value alone
- **Deprecated effects** (`deprecated-effects`, deprecations) — use of `:dispatch-n` (replaced by `:fx`)
- **Dynamic dispatch/subscribe sites** (`dynamic-sites`, info only) — dispatch or subscribe calls with a non-literal
  keyword (manual review needed)

## Example: reg-event-db returning effects

```clojure
;; BAD — reg-event-db handler returns a {:db ... :dispatch ...} map.
;; The whole map silently becomes the new app-db; :dispatch never fires.
(reg-event-db
 :cart/add-item-success
 (fn [db [_ item]]
   {:db (-> db
            (update :cart-items conj item)
            (assoc :loading? false))
    :dispatch [:analytics/track :item-added]}))

;; GOOD — switch to reg-event-fx, which expects exactly this shape.
(reg-event-fx
 :cart/add-item-success
 (fn [{:keys [db]} [_ item]]
   {:db (-> db
            (update :cart-items conj item)
            (assoc :loading? false))
    :dispatch [:analytics/track :item-added]}))
```
