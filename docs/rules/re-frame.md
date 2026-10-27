# Re-frame rules

The rules of the `re-frame` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Declarations are read from `reg-sub`, `reg-event-db`, `reg-event-fx`, `reg-event-ctx`, `reg-fx` and `reg-cofx`. Usages
are read from `subscribe`, `dispatch`, `dispatch-sync`, `:<-` signal inputs, `:fx` vector tuples (`:dispatch`,
`:dispatch-n`, `:dispatch-later`), and `:on-success` / `:on-failure` / `:on-error` http callbacks.

Run this group alone with `--only re-frame`.

## Rules

### Unused subscriptions

`unused-subs` · cleanup — A subscription nothing subscribes to.

```clojure
;; flagged
(rf/reg-sub
 ::cart-total
 (fn [db] (:total db)))

;; fix
(rf/reg-sub
 ::cart-total
 (fn [db] (:total db)))
(defn total [] @(rf/subscribe [::cart-total]))
```

### Unused events

`unused-events` · cleanup — An event nothing dispatches.

```clojure
;; flagged
(rf/reg-event-db
 ::close-panel
 (fn [db _] (assoc db :panel nil)))

;; fix
(rf/reg-event-db
 ::close-panel
 (fn [db _] (assoc db :panel nil)))
(defn close! [] (rf/dispatch [::close-panel]))
```

### Phantom subscriptions

`phantom-subs` · cleanup — A subscription to something never registered, usually a typo.

```clojure
;; flagged
(defn total [] @(rf/subscribe [::cart-totl]))

;; fix
(rf/reg-sub
 ::cart-total
 (fn [db] (:total db)))
(defn total [] @(rf/subscribe [::cart-total]))
```

### Phantom events

`phantom-events` · cleanup — A dispatch of an event never registered.

```clojure
;; flagged
(defn close! [] (rf/dispatch [::close-pannel]))

;; fix
(rf/reg-event-db
 ::close-panel
 (fn [db _] (assoc db :panel nil)))
(defn close! [] (rf/dispatch [::close-panel]))
```

### Duplicate subscriptions

`duplicate-subs` · bugs — Two subscriptions with one keyword, where the second silently replaces the first.

```clojure
;; flagged
(rf/reg-sub
 ::user
 (fn [db] (:user db)))
(rf/reg-sub
 ::user
 (fn [db] (:current-user db)))
(defn user [] @(rf/subscribe [::user]))

;; fix
(rf/reg-sub
 ::user
 (fn [db] (:current-user db)))
(defn user [] @(rf/subscribe [::user]))
```

### Duplicate events

`duplicate-events` · bugs — Two event handlers with one keyword, where the second silently replaces the first.

```clojure
;; flagged
(rf/reg-event-db
 ::reset
 (fn [db _] (dissoc db :draft)))
(rf/reg-event-db
 ::reset
 (fn [db _] (dissoc db :form)))
(defn reset-form! [] (rf/dispatch [::reset]))

;; fix
(rf/reg-event-db
 ::reset
 (fn [db _] (dissoc db :draft :form)))
(defn reset-form! [] (rf/dispatch [::reset]))
```

### `reg-event-db` returning effects

`reg-event-db-returning-effects` · bugs — A `reg-event-db` handler returning an effects map, which becomes app-db.

```clojure
;; flagged
(rf/reg-event-db
 ::saved
 (fn [db [_ id]]
   {:db (assoc db :id id)
    :dispatch [::refresh]}))

;; fix
(rf/reg-event-fx
 ::saved
 (fn [{:keys [db]} [_ id]]
   {:db (assoc db :id id)
    :dispatch [::refresh]}))
```

<details>
<summary>Details</summary>

A `reg-event-db` handler returns an effects-style `{:db ... :dispatch ...}` map. The whole map silently replaces app-db
and the extra effects are dropped; use `reg-event-fx` instead.

</details>

### `reg-event-db` empty

`reg-event-db-empty` · bugs — A `reg-event-db` handler returning `nil` or `{}`, which wipes app-db.

```clojure
;; flagged
(rf/reg-event-db
 ::reset
 (fn [_ _] {}))

;; fix
(rf/reg-event-db
 ::reset
 (fn [db _] (dissoc db :draft)))
```

<details>
<summary>Details</summary>

A `reg-event-db` handler returns `nil` or `{}`, which replaces the whole app-db with nothing. Return the db, changed; a
deliberate reset reads better as `(assoc db …)` or a named initial value.

</details>

### `reg-event-fx` empty

`reg-event-fx-empty` · bugs — A `reg-event-fx` handler returning no effects, so it does nothing.

```clojure
;; flagged
(rf/reg-event-fx
 ::noop
 (fn [_ _] {}))

;; fix
(rf/reg-event-fx
 ::noop
 (fn [_ _] {:fx [[:dispatch [::refresh]]]}))
```

<details>
<summary>Details</summary>

A `reg-event-fx` handler returns `nil` or an empty effects map, so it does nothing. Remove it, or return the effects it
was meant to.

</details>

### `reg-event-fx` db only

`reg-event-fx-db-only` · cleanup — A `reg-event-fx` handler returning only `:db`, which `reg-event-db` says simply.

```clojure
;; flagged
(rf/reg-event-fx
 ::rename
 (fn [{:keys [db]} [_ title]]
   {:db (assoc db :title title)}))

;; fix
(rf/reg-event-db
 ::rename
 (fn [db [_ title]]
   (assoc db :title title)))
```

<details>
<summary>Details</summary>

A `reg-event-fx` handler returns only `:db`. `reg-event-db` takes a handler returning the new db directly, which says
the same with less.

</details>

### `reg-sub` `:=>` with a 1-arity fn

`reg-sub-=>-1-arity` · cleanup — `:=>` with a one-argument fn, which never sees the query parameters.

```clojure
;; flagged
(rf/reg-sub
 ::last-item
 :<- [::items]
 :=> last)

;; fix
(rf/reg-sub
 ::last-item
 :<- [::items]
 :-> last)
```

<details>
<summary>Details</summary>

A `reg-sub` uses the `:=>` sugar with a one-argument fn. `:=>` calls it with the signal value and the query vector's
parameters, so in ClojureScript a one-argument fn silently drops them. Use `:->`, which passes the signal value alone.

</details>

### `reg-fx` with a public fn

`reg-fx-public-fn` · cleanup — A `reg-fx` registering a public fn of its own file, so code can call the fn and skip the
fx.

```clojure
;; flagged
(defn show-error! [response]
  (js/console.error (:status response)))
(rf/reg-fx ::show-error show-error!)
(defn on-failure [response] (show-error! response))

;; fix
(rf/reg-fx
 ::show-error
 (fn [response]
   (js/console.error (:status response))))
(rf/reg-event-fx
 ::failed
 (fn [_ [_ response]]
   {::show-error response}))
(defn on-failure [response] (rf/dispatch [::failed response]))
```

**Left alone:**

```clojure
(defn- show-error! [response]
  (js/console.error (:status response)))
(rf/reg-fx ::show-error show-error!) ; private

(ns example.core
  (:require
   [example.dom :as dom]
   [re-frame.core :as rf]))
(rf/reg-fx ::scroll dom/scroll-into-view!) ; a fn of another namespace

(defn register! [show-error!]
  (rf/reg-fx ::show-error show-error!)) ; not top-level, the symbol may be a local
```

<details>
<summary>Details</summary>

A reg-fx body is reached by writing its key into an effects map or an `:fx` vector. When it is also a public fn, code
can call it directly, a second path into the side effect that skips the fx and anything that intercepts it. Inline the
body into `reg-fx`, and where a plain fn needs the side effect, dispatch an event that returns the fx.

Only a top-level `reg-fx` whose handler is a bare symbol counts, so `#(…)`, `(partial f …)` and a wrapping `fn` are not
checked. The symbol must name a top-level `defn` of the same file. A `defn-`, a `defn` with metadata such as `^:private`
or an attr-map such as `{:private true}`, a `def`, a form behind a reader conditional, a file with more than one `ns` or
`in-ns` form, and a fn from another namespace are left alone: a fn of another namespace often has callers of its own,
and turning them into dispatches is a design choice the rule cannot make. The rule targets app code; a library may
expose its fx handler on purpose.

</details>

### Deprecated effects

`deprecated-effects` · deprecations — A deprecated `:dispatch-n` effect.

```clojure
;; flagged
(rf/reg-event-fx
 ::save
 (fn [_ _] {:dispatch-n [[::validate] [::persist]]}))

;; fix
(rf/reg-event-fx
 ::save
 (fn [_ _] {:fx [[:dispatch [::validate]] [:dispatch [::persist]]]}))
```

### Dynamic dispatch/subscribe sites

`dynamic-sites` · info-only — A dispatch or subscribe with a computed keyword, listed for review and never blocking.

```clojure
;; flagged
(defn open! [panel-event] (rf/dispatch [panel-event]))
```

**Left alone:**

```clojure
(defn open! [] (rf/dispatch [::open-panel])) ; a literal keyword
```
