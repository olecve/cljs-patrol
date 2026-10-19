# Re-frame rules

The rules of the `re-frame` group: what each one flags and what it deliberately leaves alone. Each rule carries the key
that `--fail-on` and the baseline use, and its [tier](../severity.md#tiers).

[← all rule groups](../rules.md) · [← back to the README](../../README.md)

Declarations are read from `reg-sub`, `reg-event-db`, `reg-event-fx`, `reg-event-ctx`, `reg-fx` and `reg-cofx`. Usages
are read from `subscribe`, `dispatch`, `dispatch-sync`, `:<-` signal inputs, `:fx` vector tuples (`:dispatch`,
`:dispatch-n`, `:dispatch-later`), and `:on-success` / `:on-failure` / `:on-error` http callbacks.

### Unused subscriptions

`unused-subs` · cleanup — A subscription nothing subscribes to.

```clojure
;; flagged
(rf/reg-sub ::cart-total
 (fn [db] (:total db)))

;; fix
(rf/reg-sub ::cart-total
 (fn [db] (:total db)))
(defn total [] @(rf/subscribe [::cart-total]))
```

<details>
<summary>Details</summary>

Registered with `reg-sub` but never subscribed to

</details>

### Unused events

`unused-events` · cleanup — An event nothing dispatches.

```clojure
;; flagged
(rf/reg-event-db ::close-panel
 (fn [db _] (assoc db :panel nil)))

;; fix
(rf/reg-event-db ::close-panel
 (fn [db _] (assoc db :panel nil)))
(defn close! [] (rf/dispatch [::close-panel]))
```

<details>
<summary>Details</summary>

Registered with `reg-event-*` but never dispatched

</details>

### Phantom subscriptions

`phantom-subs` · cleanup — A subscription to something never registered, usually a typo.

```clojure
;; flagged
(defn total [] @(rf/subscribe [::cart-totl]))

;; fix
(rf/reg-sub ::cart-total
 (fn [db] (:total db)))
(defn total [] @(rf/subscribe [::cart-total]))
```

<details>
<summary>Details</summary>

Subscribed to but never declared

</details>

### Phantom events

`phantom-events` · cleanup — A dispatch of an event never registered.

```clojure
;; flagged
(defn close! [] (rf/dispatch [::close-pannel]))

;; fix
(rf/reg-event-db ::close-panel
 (fn [db _] (assoc db :panel nil)))
(defn close! [] (rf/dispatch [::close-panel]))
```

<details>
<summary>Details</summary>

Dispatched but never declared

</details>

### Duplicate subscriptions

`duplicate-subs` · bugs — Two subscriptions with one keyword; the second silently replaces the first.

```clojure
;; flagged
(rf/reg-sub ::user (fn [db] (:user db)))
(rf/reg-sub ::user (fn [db] (:current-user db)))
(defn user [] @(rf/subscribe [::user]))

;; fix
(rf/reg-sub ::user (fn [db] (:current-user db)))
(defn user [] @(rf/subscribe [::user]))
```

<details>
<summary>Details</summary>

Two `reg-sub` or `reg-event-*` calls with the same keyword (second silently overwrites the first)

</details>

### Duplicate events

`duplicate-events` · bugs — Two event handlers with one keyword; the second silently replaces the first.

```clojure
;; flagged
(rf/reg-event-db ::reset (fn [db _] (dissoc db :draft)))
(rf/reg-event-db ::reset (fn [db _] (dissoc db :form)))
(defn reset! [] (rf/dispatch [::reset]))

;; fix
(rf/reg-event-db ::reset (fn [db _] (dissoc db :draft :form)))
(defn reset! [] (rf/dispatch [::reset]))
```

<details>
<summary>Details</summary>

Two `reg-sub` or `reg-event-*` calls with the same keyword (second silently overwrites the first)

</details>

### reg-event-db returning effects

`reg-event-db-returning-effects` · bugs — A `reg-event-db` handler returning an effects map, which becomes the whole
app-db.

```clojure
;; flagged
(rf/reg-event-db ::saved
 (fn [db [_ id]]
   {:db (assoc db :id id)
    :dispatch [::refresh]}))

;; fix
(rf/reg-event-fx ::saved
 (fn [{:keys [db]} [_ id]]
   {:db (assoc db :id id)
    :dispatch [::refresh]}))
```

<details>
<summary>Details</summary>

`reg-event-db` handler returns an effects-style `{:db ... :dispatch ...}` map; the whole map silently replaces app-db
and extra effects are dropped (use `reg-event-fx` instead)

</details>

### reg-event-db empty

`reg-event-db-empty` · bugs — A `reg-event-db` handler returning `nil` or `{}`, which wipes app-db.

```clojure
;; flagged
(rf/reg-event-db ::reset
 (fn [_ _] {}))

;; fix
(rf/reg-event-db ::reset
 (fn [db _] (dissoc db :draft)))
```

<details>
<summary>Details</summary>

A `reg-event-db` handler returns `nil` or `{}`, which replaces the whole app-db with nothing. Return the db, changed; a
deliberate reset reads better as `(assoc db …)` or a named initial value

</details>

### reg-event-fx empty

`reg-event-fx-empty` · bugs — A `reg-event-fx` handler returning no effects, so it does nothing.

```clojure
;; flagged
(rf/reg-event-fx ::noop
 (fn [_ _] {}))

;; fix
(rf/reg-event-fx ::refresh-all
 (fn [_ _] {:fx [[:dispatch [::refresh]]]}))
```

<details>
<summary>Details</summary>

A `reg-event-fx` handler returns `nil` or an empty effects map, so it does nothing. Remove it, or return the effects it
was meant to

</details>

### reg-event-fx db only

`reg-event-fx-db-only` · cleanup — A `reg-event-fx` handler returning only `:db`, which `reg-event-db` says more simply.

```clojure
;; flagged
(rf/reg-event-fx ::rename
 (fn [{:keys [db]} [_ title]]
   {:db (assoc db :title title)}))

;; fix
(rf/reg-event-db ::rename
 (fn [db [_ title]]
   (assoc db :title title)))
```

<details>
<summary>Details</summary>

A `reg-event-fx` handler returns only `:db`. `reg-event-db` takes a handler returning the new db directly, which says
the same with less

</details>

### reg-sub :=> with a 1-arity fn

`reg-sub-=>-1-arity` · cleanup — `:=>` with a one-argument fn, which never sees the query vector.

```clojure
;; flagged
(rf/reg-sub ::last-item
 :<- [::items]
 :=> last)

;; fix
(rf/reg-sub ::last-item
 :<- [::items]
 :-> last)
```

<details>
<summary>Details</summary>

`reg-sub` uses the `:=>` sugar with a one-argument fn. `:=>` calls it with the signal value and the query vector, so in
ClojureScript the query vector is silently dropped. Use `:->`, which passes the signal value alone

</details>

### Deprecated effects

`deprecated-effects` · deprecations — The deprecated `:dispatch-n` effect.

```clojure
;; flagged
(rf/reg-event-fx ::save
 (fn [_ _] {:dispatch-n [[::validate] [::persist]]}))

;; fix
(rf/reg-event-fx ::save
 (fn [_ _] {:fx [[:dispatch [::validate]] [:dispatch [::persist]]]}))
```

<details>
<summary>Details</summary>

Use of `:dispatch-n` (replaced by `:fx`)

</details>

### Dynamic dispatch/subscribe sites

`dynamic-sites` · info only — A dispatch or subscribe whose keyword is computed, listed for review; never blocks.

```clojure
;; flagged
(defn open! [panel-event] (rf/dispatch [panel-event]))
```

**Left alone:**

```clojure
(defn open! [] (rf/dispatch [::open-panel]))
```

<details>
<summary>Details</summary>

Dispatch or subscribe calls with a non-literal keyword (manual review needed)

</details>
