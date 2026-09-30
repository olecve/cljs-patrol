# What cljs-patrol detects

Every rule, what it flags, and what it deliberately leaves alone. For the one-line summary of each rule group, see
[Rule groups](../README.md#rule-groups).

[← back to the README](../README.md)

- **Unused subscriptions** — registered with `reg-sub` but never subscribed to
- **Unused events** — registered with `reg-event-*` but never dispatched
- **Unused styles** — declared with `defclass`/`defattrs` but never called
- **Phantom subscriptions** — subscribed to but never declared
- **Phantom events** — dispatched but never declared
- **Duplicate registrations** — two `reg-sub` or `reg-event-*` calls with the same keyword (second silently overwrites
  the first)
- **reg-event-db returning effects** — `reg-event-db` handler returns an effects-style `{:db ... :dispatch ...}` map;
  the whole map silently replaces app-db and extra effects are dropped (use `reg-event-fx` instead)
- **Deprecated effects** — use of `:dispatch-n` (replaced by `:fx`)
- **defclass as sole attr** — `defclass` where every usage is `{:class (style-fn)}` with no other props; should be
  `defattrs` instead
- **Redundant into-hiccup** — `(into [:tag …] (map …))` or `(into [component …] (for …))` where the mapping body already
  attaches a `:key` to each element it produces. React then has the keys it needs without the `into` splicing the
  elements in as positional children, so the sequence can sit inside the Hiccup vector:
  `[:<> (for [x xs] ^{:key x} [item x])]`. A key counts in any of its three live forms — reader metadata `^{:key k}`, an
  explicit `(with-meta … {:key k})`, and `:key` in the props map of an element the body writes out. Three shapes are
  left alone on purpose:
  - **A keyless sequence** — `(into [:<>] (map render) nodes)`. There the `into` is load-bearing: it splices the
    elements in as positional children, which React accepts, where the same sequence inside the vector draws a
    missing-key warning
  - **A vector that is not Hiccup** — the head keyword has to name an HTML/SVG element or the `:<>` fragment.
    `(into [:enum] sections)` is a Malli schema, `(into [:cart] ks)` a re-frame db path; `:map` is excluded from the tag
    set, since `[:map {:closed true} …]` is a schema far more often than it is the HTML `<map>` element
  - **A body that derefs** — `(into [:span] (map-indexed (fn [i seg] … @state …) segs))`. Reagent's caveat is a ratom
    deref inside a lazy seq, and the eager `into` is what keeps it out of one

  A `:key` in the props of the container being spliced into — `(into [:div {:class c :key k}] children)` — belongs to
  the container, not to its children, and is not read as one

- **defattrs in merge** — `defattrs` used inside `merge`; should be `defclass` so callers can pass it via `:class`
  without merge
- **Mixed typography token groups** — typography tokens from different Figma token groups mixed in a single style
  definition
- **`:img` missing `:alt`** — `[:img {...}]` without an `:alt` attribute; use `:alt ""` for decorative images
- **Invalid tabindex** — `:tabIndex`/`:tab-index` with a value that isn't `0` or a negative integer (positive ints break
  natural focus order; non-int literals aren't valid tabindex values)
- **`:on-click` on non-interactive tag** — `:on-click` on `:div`/`:span`/`:svg`/`:li`/`:p`/`:section` etc. without
  `:role` or a keyboard handler; keyboard users can't activate it. `:svg` counts because an icon rendered as a bare
  `<svg>` carrying its own handler is not focusable and has no button semantics. The handler is found even when the
  props are built rather than written out — `(assoc base :on-click f)`, `(merge base {:on-click f})`,
  `(assoc-in base [:on-click] f)` — since those calls name the key literally. Only what the call makes readable counts,
  so a props map that is wholly opaque (`[icon (build-props)]`) is still left alone, and a base the file itself defines
  is read for the role and keyboard handler it carries (see [Props named by a symbol](#props-named-by-a-symbol))
- **Empty interactive element** — `:button`, `:a`, or `:role "button"`/`"link"` with no visible text and no
  `:aria-label`/`:aria-labelledby`/`:title`; screen readers announce nothing. A body that is only a
  `(cond …)`/`(if …)`/`(when …)` whose every branch renders an icon counts as no visible text, so an icon-only toggle is
  caught rather than mistaken for content. Non-empty `:alt` on a child `[:img …]` does name the control, and
  `:aria-pressed`/`:aria-checked`/`:role "checkbox"` mark a stateful widget and are left alone
- **Missing accessible name** — native `[:textarea …]`, native `[:dialog …]`, any hiccup vector whose props carry
  `:role "dialog"` / `:role :dialog` / `:role "alertdialog"` / `:aria-modal true`, any vector carrying one of the
  container roles WAI-ARIA marks _Accessible Name Required_ — `:role "listbox"`, `"grid"`, `"tree"` — which a screen
  reader announces on entry with nothing but the name to say which container the user has landed in (a `[:caption …]`
  child names its table, as HTML-AAM specifies, so `[:table {:role "grid"} [:caption …] …]` is not flagged),
  `:role "tablist"` / `"menu"` / `"menubar"` — stricter than WAI-ARIA, which permits these three to go unnamed; they are
  announced on entry exactly as the others are, and two on one page are indistinguishable without a name — and any
  wrapper component (`[my.ui/textarea …]`, `[my.ui/drawer …]`) mapped in
  [`:a11y :component-aliases`](#a11y-component-aliases), that lacks `:aria-label` or `:aria-labelledby`; `:placeholder`
  is a hint, not a name. Props named by a symbol are read through the `let` / `when-let` / `if-let` or the `def` that
  names them (see [Props named by a symbol](#props-named-by-a-symbol)), so a shared props binding is judged by the map
  it holds
- **Repeated accessible name** — a control rendered once per collection item names every item with the same literal
  string, so a screen-reader user listing the page's controls hears "Remove, Remove, Remove" with nothing to tell them
  apart. Flagged on `:button` / `:a` / `:role "button"` / `:role "link"` carrying a literal `:aria-label` inside `for` /
  `map` / `mapv` / `mapcat` / `map-indexed` / `keep` / `keep-indexed`, and on `:img` carrying a literal `:alt` when it
  is the name of an enclosing control that supplies none itself. Left alone: a computed name like
  `(str "Figure " (inc i))`, which already varies; `:alt ""` on a decorative image; `:alt` on an image that names
  nothing, such as a status badge in a row; a built attrs map like `(assoc base :aria-label "…")` whose `base` cannot be
  read, since it may still supply a per-item name — one built entirely from maps this can read holds no such surprise,
  and is flagged; the one expression a `for` evaluates once, its first binding's collection; a name that something else
  overrides, since `:aria-labelledby` wins the name computation and `:aria-hidden true` removes the element from the
  tree; and repeated visible body text, since WCAG lets a control take its purpose from its surroundings where an
  authored `:aria-label` does not
- **`aria-hidden` on a focusable element** — a hiccup vector carrying `:aria-hidden true` (or `"true"` / `:true`) that
  keyboard or mouse focus can still reach, on the element itself or on something inside it — `:aria-hidden` applies to
  the whole subtree, so a hidden wrapper keeps every tab stop under it, and the report names the element that still
  takes focus. Assistive technology is told the element is not there, so a screen-reader user who tabs onto it hears
  nothing at all. Triggered by a natively focusable tag (`:button`, `:input`, `:textarea`, `:select`, `:details`,
  `:summary`, and `:a` carrying an `:href`), by a widget `:role` (`"button"`, `"link"`, `"checkbox"`, `"radio"`,
  `"switch"`, `"tab"`, `"option"`, `"menuitem"`, `"combobox"`, `"slider"`, …), or by a non-negative `:tabIndex` /
  `:tab-index`. A literal negative tabindex is the escape hatch and stops the report: that is what takes a mouse-only
  affordance out of the tab order, and it should be paired with an `:on-mouse-down` calling `.preventDefault` to block
  mouse focus as well. Non-widget roles are not triggers — a decorative `[:svg {:aria-hidden true}]` is correct markup.
  Every way out has to be readable before one can be called missing, so nothing is reported when the attrs map is
  computed, when it is built on a base that cannot be read (`(assoc props :aria-hidden true)`), when the tabindex itself
  cannot be read — the roving `:tab-index (if active? 0 -1)` that every treeitem, tab and option widget uses — or when a
  form control is `:disabled` and so out of the tab order already. `disabled` counts only on the tags it governs
  (`:button`, `:input`, `:select`, `:textarea`, `:fieldset`, `:optgroup`, `:option`); on a `:div` or an `:a` it is inert
  and the tab stop survives it. A child whose own props are named by a symbol is passed over for the same reason the
  element's would be. A built map every part of which _is_ readable states its whole key set and is treated as written
  out. An `:href` or `:disabled` whose literal value is `nil` or `false` counts as absent, since Reagent omits those
  attributes
- **Label not associated** — a `[:label …]` that labels nothing that renders. HTML gives a label its control two ways:
  the `for` attribute naming one by id, or the first labelable element among its descendants (`:input` that is not
  hidden, `:select`, `:textarea`, `:button`, `:meter`, `:output`, `:progress`). ARIA gives it a third, running the other
  way — a control pointing `aria-labelledby` at the label's id, which the rule finds by scanning the file. With none of
  them the element is text that happens to be a `<label>`: clicking it focuses nothing and it contributes no accessible
  name, however much a sighted reader takes it for the field's label. Naming the field with `:aria-label` instead is not
  a fix — the visible text and the announced name then say different things, which a speech-input user cannot bridge
  (WCAG 2.5.3 Label in Name). A component among the label's children **ends** the check rather than failing it, since a
  wrapper may render the control. A props call that cannot be read does **not** end it: `for` holds one control's id, so
  a call shared between call sites could only return a constant one, which would point every label it renders at the
  same element. `(assoc (styles/field) :for id)` is read properly and passes. `[:label string?]` in a Malli schema is
  not markup and is skipped — props, or a string written out, are what tell the two apart, so a label whose only child
  is a bare symbol is left alone for the same reason
- **Nested interactive element** — an interactive element containing another one. The HTML content model bans
  interactive content inside `<button>` and inside `<a href>`; React logs a `validateDOMNesting` warning for it, and
  browsers recover by restructuring the markup differently from one another. The ARIA form of the same mistake —
  `:role "button"` or `:role "link"` on a wrapper holding real controls — gives assistive technology two overlapping
  controls to announce and leaves keyboard activation ambiguous. The element's **body** is searched, not its props: a
  Hiccup vector handed over as a prop (`[:button {:tooltip [:a …]} …]`) is markup the element passes on, not markup
  nested inside it, and a props map built by a call (`[:button (build-props) …]`) is read the same way. Below the body
  the whole subtree counts, so a control under layout `:div`s or produced by a `for` is found; quoted or discarded
  markup is not, being data rather than something that renders, and markup living in another component stays invisible.
  Interactive content is the HTML category — `:button`, `:input` that is not hidden, `:select`, `:textarea`, `:label`,
  `:details`, `:embed`, `:iframe`, and `:a` carrying an `:href` — plus anything claiming a button or link role. An `:a`
  whose `:href` is absent or literally `nil` is not interactive and neither nests nor counts as nested. Fix by making
  the wrapper a plain `:div` that does the positioning and the two controls siblings, or by dropping the outer one
- **`aria-live` contradicts role** — a hiccup vector whose props set `:aria-live` to a different politeness than its
  `:role` implies (`"status"` and `"log"` imply `"polite"`, `"alert"` implies `"assertive"`). The attribute wins:
  browsers read `:aria-live` first and fall back to the role only when it is absent, so `:role "alert"` with
  `:aria-live "polite"` is an alert silently demoted to polite. A role carrying no `:aria-live` at all is **not**
  flagged — it is conformant markup, and on `:role "alert"` the redundant attribute is documented to double-speak in
  VoiceOver on iOS. `:aria-live "off"` is not flagged either; silencing a live region is a deliberate choice
- **Pseudo-selector in Spade main map** — `defclass`/`defattrs` with a `:&`-prefixed key (e.g. `:&:hover`) inside a
  style map; Spade emits it as an invalid CSS property and silently drops the rule. Move the selector into its own
  sibling vector `[:&:hover {…}]`. Checked in the base map and in the map of every nested selector block, at any depth
- **Consecutive self-selectors** — a Spade selector vector begins with 2+ `:&`-prefixed keywords (e.g.
  `[:&:before :&:after {…}]`); Garden compiles this as a descendant selector (`elem:before elem:after`), not the
  comma-joined selector the author intended. Checked at every nesting depth, not only on the declaration's own siblings
- **Ampersand not at start** — a string selector inside `defclass`/`defattrs` carries `&` anywhere but position 0 (e.g.
  `["li:focus-within &" {…}]`). Garden substitutes the parent-class reference only at the front of a string selector;
  elsewhere the `&` survives into the stylesheet as a literal character, and the rule matches nothing. Checked at every
  nesting depth; `"&"`, `"&:hover"` and `"&[data-x]"` are correct
- **Keyword combinator selector** — a Spade selector vector holds a combinator keyword (`:>`, `:+`) alongside another
  selector, e.g. `[:> :span {…}]`. Garden reads a selector vector as a comma-separated list, so this compiles to
  `.parent >, .parent span {…}` — the first half invalid, the second matching every descendant. Use the string form,
  `["> span" {…}]`. Single-element vectors (`[:&:hover {…}]`), string selectors and plain descendant chains
  (`[:svg :path {…}]`) are left alone
- **CSS property order (outside-in)** — a Spade style map writes its properties out of the order the chosen table
  defines. Each style map is judged on its own — the base map and every nested selector block, at any depth — and only
  maps written as literals in the declaration body are read, so a map that `(merge …)` or `(case …)` builds is left
  alone. A property the table does not name, a custom property (`--*`) included, is unordered; it only reads as a
  problem when a ranked property follows it. Maps under four ranked properties are not judged, and only the first
  property out of place in each block is reported — alongside the whole block's target order, so the fix needs no
  guessing. See [Property-order tables](#property-order-tables) for the choice of table
- **Docstring summary** — first line of a multi-line docstring is not a self-contained sentence ending in `.`, `!`, `?`,
  or `:`
- **Docstring indentation** — continuation lines of a multi-line docstring are indented less than the opening-quote
  column
- **Docstring leading/trailing whitespace** — docstring starts or ends with whitespace
- **Dynamic dispatch/subscribe sites** — dispatch or subscribe calls with a non-literal keyword (manual review needed)

## Property-order tables

The `css-order` group ranks properties with one of five [stylelint](https://stylelint.io) property-order configs, each
embedded verbatim from its npm package. These are the same lists
[`stylelint-order`](https://github.com/hudochenkov/stylelint-order) applies to CSS, so a project already running one of
them in stylelint gets the matching order here.

| Table              | Package                                                                                                                  | Properties | Character                                                                             |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------ | ---------- | ------------------------------------------------------------------------------------- |
| `recess` (default) | [stylelint-config-recess-order](https://github.com/stormwarning/stylelint-config-recess-order)                           | 496        | Positioning, box model, **typography, then background and border**, effects           |
| `clean`            | [stylelint-config-clean-order](https://github.com/kutsan/stylelint-config-clean-order)                                   | 468        | Interaction, positioning, layout, box model (border included), typography, appearance |
| `concentric`       | [stylelint-config-concentric-order](https://github.com/ream88/stylelint-config-concentric-order)                         | 330        | Outside-in: **border and background, then the text inside**                           |
| `smacss`           | [stylelint-config-property-sort-order-smacss](https://github.com/cahamilton/stylelint-config-property-sort-order-smacss) | 225        | Grouped by purpose: content, box, animation, border, background, text                 |
| `idiomatic`        | [stylelint-config-idiomatic-order](https://github.com/ream88/stylelint-config-idiomatic-order)                           | 64         | Structural properties only; the rest left unranked                                    |

They disagree most about where layout ends and looks begin: `recess` puts typography ahead of background and border,
`concentric` does the opposite. Read a finding against the table in use.

Pick one on the command line:

```bash
clojure -M:run --css-order smacss <src-dir>
```

or in `.cljs-patrol/config.edn`, where it persists for the project and for CI:

```clojure
{:css-order {:order :smacss}}
```

An unrecognized name warns on stderr and falls back to `recess` rather than stopping the run.

Every finding carries the target order for its own block, so neither a reader nor another tool has to work out what the
table wants:

```
styles.cljs:16  :app.ui/banner-style {:color "#333" :display :block :padding "4px" :background "#eee"}
      → Move :display before :color. Order for this block: :display :padding :color :background
```

The console and HTML reports print that line; the EDN report carries the full sequence as `:expected-order`, which stays
complete even where the printed hint trails off.

Two deviations from `stylelint-order` worth knowing. A property the chosen table does not name is left unordered here;
`idiomatic` in particular asks stylelint to sort its unlisted properties alphabetically at the bottom, which this does
not do. And nothing is auto-fixed — property order is a write-time review decision.

## A11y component aliases

`:missing-accessible-name` and the other a11y rules only inspect native HTML tags (`[:textarea …]`, `[:button …]`) by
default. Real codebases usually wrap those in a component library — `[my.ui/textarea …]`, `[my.ui/button …]` — which
then slips past every check.

Map each wrapper to the native tag it renders in `.cljs-patrol/config.edn`:

```edn
{:a11y {:component-aliases
        {my.ui/button :button
         my.ui/drawer :dialog
         my.ui/textarea :textarea
         my.ui.icons/* :svg}}}
```

A `some.ns/*` key maps every var in that namespace, so a 200-icon namespace costs one line instead of 200. An exact
symbol key wins over the glob when both match.

Any call whose head symbol resolves (via `:as` or `:refer` in the caller's `ns`) to a mapped fully-qualified symbol is
then checked as if it were the native tag. `[my.ui/textarea {:placeholder "…"}]` participates in
`:missing-accessible-name`; icon-only `[my.ui/button [icons/x]]` participates in `:empty-interactive-element`;
`[my.ui/drawer {:open? true}]` participates in `:missing-accessible-name` via the `:dialog` mapping;
`[my.ui.icons/x {:on-click f}]` participates in `:on-click-on-non-interactive` via the `:svg` mapping;
`[my.ui/button {:aria-label "Remove"}]` inside a `for` participates in `:repeated-accessible-name`. All existing a11y
rules compose the same way.

## Props named by a symbol

Shared props are usually named once and reused:

```clojure
(defn notifications-popover []
  (let [content-props {:aria-label "Notifications"
                       :side :top}]
    [my.ui/popover-content content-props
     [notification-list]]))
```

Every a11y rule follows a symbol in the props slot to the form that names it — `let`, `let*`, `when-let`, `if-let`,
`when-some`, `if-some`, or a `def` in the same file — and reads a map literal found there exactly as if it had been
written inline. So the popover above is named, and one bound to a map without `:aria-label` is still reported.

Resolution is innermost-first, and anything that rebinds the name ends the search rather than letting an outer binding
answer for a symbol it no longer names — a parameter list, a destructuring form, a `for` or `loop` binding vector, a
`catch` name. Where a rebinding cannot be read with confidence the symbol is left unknown, which costs a finding rather
than inventing one. Nothing crosses a namespace: a symbol another file defines stays unknown.

A binding to a map-building call is read for the keys it names — `(merge base {:aria-label "…"})` bound in a `let`
answers the same way it does written in the slot, and `(merge defaults opts)`, which names none, holds an unknown map
rather than an empty one — while a symbol bound to anything else, an opaque call or another symbol, leaves the props
slot as opaque as it was before.

How much a built map can be asked depends on how much of it can be read. `(assoc base :on-click f)` over an opaque
`base` is a **floor**: it answers that `:on-click` is there, never that `:role` is missing, so rules that report
something absent leave it alone. Once every part is readable — a literal base, or a symbol a `let` or a `def` in the
same file names a map literal with — the call states the **whole** map, and it is read exactly like a map written out,
absence and all:

```clojure
(def clickable-icon-props {:role "button" :tabIndex 0 :on-key-down activate})

;; reads the role and the keyboard handler out of the def — not an :on-click-on-non-interactive
[my.ui.icons/x (assoc clickable-icon-props :on-click on-select)]

;; base is a parameter, so only :on-click is knowable — still reported
(defn icon [base on-select]
  [my.ui.icons/x (assoc base :on-click on-select)])
```

## Example: reg-event-db returning effects

```clojure
;; BAD — reg-event-db handler returns a {:db ... :dispatch ...} map.
;; The whole map silently becomes the new app-db; :dispatch never fires.
(reg-event-db
 :cart/add-item-success
 (fn [{:keys [db]} [_ item]]
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
