(ns cljs-patrol.groups.reagent-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.reagent :as reagent]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(def ^:private library-aliases {:aliases {"rf" "re-frame.core"
                                          "r" "reagent.core"
                                          "rum" "rum.core"
                                          "s" "schema.core"}
                                :refers {"dispatch" "re-frame.core"}})

(def ^:private defclass-sole {:kw :webapp.styles/sole-style
                              :type :defclass
                              :file "styles.cljs"
                              :row 9})
(def ^:private class-only-usage {:kw :webapp.styles/sole-style
                                 :type :style-call
                                 :file "views.cljs"
                                 :row 14
                                 :context :class-only-map})

(deftest analyze-test
  (testing "flags defclass used only in class-only-map"
    (let [result (group/analyze reagent/group
                                {:declarations [defclass-sole]
                                 :usages [class-only-usage]})]
      (is (= 1 (count (:defclass-as-sole-attr result))))
      (is (= :webapp.styles/sole-style (-> result :defclass-as-sole-attr first :kw)))))

  (testing "not flagged when any usage is not class-only-map"
    (let [mixed-usage {:kw :webapp.styles/sole-style
                       :type :style-call
                       :file "v.cljs"
                       :row 20
                       :context nil}
          result (group/analyze reagent/group
                                {:declarations [defclass-sole]
                                 :usages [class-only-usage mixed-usage]})]
      (is (empty? (:defclass-as-sole-attr result)))))

  (testing "not flagged when defclass has no usages"
    (let [result (group/analyze reagent/group
                                {:declarations [defclass-sole]
                                 :usages []})]
      (is (empty? (:defclass-as-sole-attr result)))))

  (testing "surfaces redundant-into-hiccup findings from declarations"
    (let [into-finding {:kw :ul
                        :type :redundant-into-hiccup
                        :form "(into [:ul] (for [x xs] [:li x]))"
                        :file "views.cljs"
                        :row 12
                        :col 3}
          result (group/analyze reagent/group
                                {:declarations [into-finding]
                                 :usages []})]
      (is (= [into-finding] (:redundant-into-hiccup result))))))

(deftest failed?-test
  (is (not (group/failed? reagent/group {:defclass-as-sole-attr [defclass-sole]}))
      "cleanup rule alone does not fail")
  (is (group/failed? reagent/group {:discarded-hiccup [{:type :discarded-hiccup}]})
      "discarded hiccup fails"))

(deftest summary-lines-test
  (let [lines (group/summary-lines reagent/group
                                   {:defclass-as-sole-attr [defclass-sole]
                                    :redundant-into-hiccup []
                                    :empty-fragment-props []
                                    :redundant-fragment []
                                    :inline-component []
                                    :discarded-hiccup []
                                    :remounting-branch []})]
    (is (= 7 (count lines)))
    (is (= 1 (second (first lines))))
    (is (= 0 (second (second lines))))
    (is (= [["Empty fragment props:" 0]
            ["Redundant fragment:" 0]
            ["Inline component:" 0]
            ["Discarded hiccup:" 0]
            ["Remounting branch:" 0]]
           (take-last 5 lines)))))

(defn- vector-findings
  "Walk `lines` the way the parser walks a file, and return the forms `rule` reports."
  [rule lines]
  (let [{:keys [handle-vector]} (group/parse-handlers reagent/group)
        declarations (loop [loc (z/of-string (str/join "\n" lines))
                            found []]
                       (if (z/end? loc)
                         found
                         (recur (z/next loc)
                                (if (= :vector (z/tag loc))
                                  (into found (:decls (handle-vector loc library-aliases "views.cljs")))
                                  found))))]
    (->> (group/analyze reagent/group {:declarations declarations
                                       :usages []})
         rule
         (mapv :form))))

(defn- empty-fragment-props [lines] (vector-findings :empty-fragment-props lines))

(defn- redundant-fragments
  "Return the forms the rule reports for `lines`, each written as the body of a function."
  [lines]
  (vector-findings :redundant-fragment (map #(str "(defn view [] " % ")") lines)))

(deftest fragment-with-empty-props-is-flagged-whatever-follows
  (is (= ["[:<> {} [edit-controls {:editing? false}] [accept-button]]"
          "[:<> {} (when unsaved? [:span \"Unsaved\"])]"
          "[:<> {} (when cancel? [cancel-button]) [accept-button]]"
          "[:<> {}]"
          "[:<> {} \"Saved\"]"
          "[:<> {} nil [row]]"
          "[:<> {} busy-reason [spinner]]"
          "[:<> {} (render-children items)]"
          "[:<> {} (for [x xs] ^{:key x} [row x])]"
          "[:<> {} {:a 1}]"
          "[:<> {} #_old [row]]"
          "[:<> #_x {} [row]]"]
         (empty-fragment-props ["[:<> {}\n [edit-controls {:editing? false}]\n [accept-button]]"
                                "[:<> {}\n (when unsaved?\n   [:span \"Unsaved\"])]"
                                "[:<> {}\n (when cancel?\n   [cancel-button])\n [accept-button]]"
                                "[:<> {}]"
                                "[:<> {} \"Saved\"]"
                                "[:<> {} nil [row]]"
                                "[:<> {} busy-reason [spinner]]"
                                "[:<> {} (render-children items)]"
                                "[:<> {} (for [x xs] ^{:key x} [row x])]"
                                "[:<> {} {:a 1}]"
                                "[:<> {} #_old [row]]"
                                "[:<> #_x {} [row]]"]))))

(deftest fragment-with-real-or-no-props-is-left-alone
  (is (empty? (empty-fragment-props ["[:<> {:key id} [row item]]"
                                     "[:<> [row item]]"
                                     "[:<> nil [row]]"
                                     "[:<> ^:x {} [row]]"
                                     "[:div {} [row]]"
                                     "[:> Fragment {} [row]]"]))))

(deftest unrendered-fragment-is-left-alone
  (is (empty? (empty-fragment-props ["'[:<> {} [row]]"
                                     "#_[:<> {} [row]]"
                                     "(defclass panel [] [:<> {} [row]])"
                                     "(ns app.views (:require [:<> {} [row]]))"]))))

(deftest empty-fragment-props-finding-names-the-fragment-and-where-it-is
  (let [{:keys [handle-vector]} (group/parse-handlers reagent/group)
        loc (-> (z/of-string "(defn f []\n  [:<> {} [row]])") z/down z/rightmost)]
    (is (= {:kw :<>
            :type :empty-fragment-props
            :form "[:<> {} [row]]"
            :file "views.cljs"
            :row 2
            :col 3}
           (first (:decls (handle-vector loc {} "views.cljs")))))))

(deftest fragment-around-single-element-is-flagged
  (is (= ["[:<> [:section.company-preferences [:h2 \"Links\"]]]"
          "[:<> [patent-documents-panel {:aria-label label}]]"
          "[:<> [ui/panel]]"
          "[:<> [row] #_[old-row]]"
          "[:<> [:<> [row] [cell]]]"]
         (redundant-fragments ["[:<>\n [:section.company-preferences\n  [:h2 \"Links\"]]]"
                               "[:<> [patent-documents-panel {:aria-label label}]]"
                               "[:<> [ui/panel]]"
                               "[:<> [row] #_[old-row]]"
                               "[:<> [:<> [row] [cell]]]"]))))

(deftest fragment-that-is-not-redundant-is-left-alone
  (is (empty? (redundant-fragments ["[:<> [row] [cell]]"
                                    "[:<> (for [x xs] ^{:key x} [row x])]"
                                    "[:<> (when ready? [row])]"
                                    "[:<> children]"
                                    "[:<> \"Saved\"]"
                                    "[:<>]"
                                    "[:<> ^{:key 1} [row]]"
                                    "[:<> [:path :to :key]]"
                                    "[:<> [1 2]]"
                                    "[:<> #?(:cljs [row])]"
                                    "[:div [row]]"]))
      "a child that may be a map or no element, more than one child, or data"))

(deftest fragment-carrying-a-key-is-left-alone
  (is (empty? (redundant-fragments ["[:<> {:key id} [row]]"
                                    "(for [x xs] ^{:key x} [:<> [row x]])"
                                    "(map #(with-meta [:<> [row %]] {:key %}) xs)"
                                    "(with-meta [:<> [row]] {:key 1})"]))))

(deftest fragment-where-its-value-renders-is-flagged
  (is (= ["[:<> [panel {:mode :search}]]"
          "[:<> [row]]"
          "[:<> [row]]"
          "[:<> [row x]]"
          "[:<> [row %]]"
          "[:<> [row]]"
          "[:<> [row]]"
          "[:<> [:span title]]"
          "[:<> [row]]"
          "[:<> [row]]"
          "[:<> [row]]"
          "[:<> [row]]"]
         (redundant-fragments ["(cond searching? [:<> [panel {:mode :search}]] :else [panel {:mode :empty}])"
                               "(defn view [] [:<> [row]])"
                               "(defn view [] (let [n 1] [:<> [row]] #_old))"
                               "(for [x xs] [:<> [row x]])"
                               "(map (fn [%] [:<> [row %]]) xs)"
                               "(defmethod view :a [_] [:<> [row]])"
                               "(when ready? [:<> [row]])"
                               "[card {:title [:<> [:span title]]}]"
                               "[:div [header] [:<> [row]]]"
                               "(fn [] [:<> [row]] #_old)"
                               "(let [n 1] (cond a [:<> [row]] :else [cell]))"
                               "(r/with-let [n 1] (if a [:<> [row]] [cell]))"]))
      "a branch that yields the child bare remounts it today, which an explicit key should say"))

(deftest fragment-handed-to-a-call-is-left-alone
  (is (empty? (redundant-fragments ["(into [:<> [header]] (for [x xs] ^{:key x} [row x]))"
                                    "(conj [:<> [header]] [footer])"
                                    "(-> [:<> [header]] (conj [footer]))"
                                    "(is (= [:<> [:div \"x\"]] (view)))"
                                    "(defn view [] [:<> [row]] (log))"
                                    "(defn view ([] [:<> [row]]))"
                                    "#{[:<> [row]]}"
                                    "(into (when c [:<> [a]]) rows)"
                                    "(into [:div] (if x [:<> [a]] [[b]]))"
                                    "(with-meta (when c [:<> [a]]) {:key k})"
                                    "(r/as-element (if c [:<> [a]] [b]))"
                                    "(let [content [:<> [panel]]] [:div content])"
                                    "(for [x xs :let [y [:<> [a]]]] y)"
                                    "(rf/dispatch [:open-modal [:<> [modal-body]]])"
                                    "[[:<> [a]] [:<> [b]]]"
                                    "(is (= [:div [:<> [:span \"x\"]]] (view)))"
                                    "{[:<> [x]] 1}"
                                    "(foo {:content [:<> [x]]})"
                                    "(into [:div] (for [x xs] [:<> [row x]]))"
                                    "[:path :to [:<> [row]]]"
                                    "(if (let [x [:<> [row]]] x) [a] [b])"
                                    "(when [:<> [row]] [a])"]))
      "data the call may change, or no value the form returns"))

(deftest fragment-under-rum-defc-is-flagged
  (is (= ["[:<> [row]]"]
         (vector-findings :redundant-fragment ["(rum/defc view [] [:<> [row]])"]))
      "reads rum defc as function body"))

(deftest fragment-under-look-alike-heads-is-left-alone
  (is (empty? (vector-findings :redundant-fragment ["(x/defn view [] [:<> [row]])"
                                                    "(defn view [] (x/when c [:<> [row]]))"
                                                    "(defn view [] (x/if c [:<> [row]] [cell]))"
                                                    "(defn view [] (x/for [x xs] [:<> [row x]]))"
                                                    "(let [defn f] (defn view [] [:<> [row]]))"]))
      "skips heads resolving outside core, reagent and rum"))

(deftest unrendered-redundant-fragment-is-left-alone
  (is (empty? (redundant-fragments ["'[:<> [row]]"
                                    "#_[:<> [row]]"
                                    "(defclass panel [] [:<> [:div]])"
                                    "(ns app.views (:require [:<> [row]]))"]))))

(defn- list-findings
  "Walk `lines` the way the parser walks a file, and return `field` of each finding `rule` gives."
  [rule field ns-info lines]
  (let [handle-lists (:handle-list (group/parse-handlers reagent/group))
        declarations (loop [loc (z/of-string (str/join "\n" lines))
                            found []]
                       (if (z/end? loc)
                         found
                         (recur (z/next loc)
                                (if (#{:list :fn} (z/tag loc))
                                  (into found (mapcat #(:decls (% loc ns-info "views.cljs")) handle-lists))
                                  found))))]
    (->> (group/analyze reagent/group {:declarations declarations
                                       :usages []})
         rule
         (mapv field))))

(defn- inline-components [lines] (list-findings :inline-component :hint library-aliases lines))

(defn- discarded-hiccup
  ([lines] (discarded-hiccup {} lines))
  ([ns-info lines] (list-findings :discarded-hiccup :form (merge library-aliases ns-info) lines)))

(deftest let-binding-holding-several-handler-elements-is-flagged
  (is (= ["actions holds 2 elements with event handlers. Make it a component of its own."
          "content holds 3 elements with event handlers. Make it a component of its own."
          "panel holds 2 elements with event handlers. Make it a component of its own."
          "box holds 2 elements with event handlers. Make it a component of its own."
          "controls holds 2 elements with event handlers. Make it a component of its own."
          "row holds 2 elements with event handlers. Make it a component of its own."
          "listed holds 2 elements with event handlers. Make it a component of its own."
          "mapped holds 2 elements with event handlers. Make it a component of its own."
          "kept holds 2 elements with event handlers. Make it a component of its own."
          "maybe holds 2 elements with event handlers. Make it a component of its own."
          "keyed holds 2 elements with event handlers. Make it a component of its own."
          "keyed-row holds 2 elements with event handlers. Make it a component of its own."
          "trailing holds 2 elements with event handlers. Make it a component of its own."]
         (inline-components [(str "(let [actions [:div [button {:on-click save} \"Save\"]"
                                  " [button {:on-click cancel} \"Cancel\"]]] actions)")
                             "(let [content [:<> [a {:on-change f}] [b {:on-change g}] [c {:on-select h}]]] content)"
                             (str "(let [panel [:div [:section [button {:on-click a} \"A\"]]"
                                  " [:footer [:div [:input {:on-change b}]]]]] panel)")
                             "(let [box [:div {:on-click a} [button {:on-click b} \"B\"]]] box)"
                             "(r/with-let [controls [:div [b {:on-click a}] [b {:on-click c}]]] controls)"
                             "(let [row [:div [b {:on-click a}] (when ok? [b {:on-click c}])]] row)"
                             "(let [listed [:div [b {:on-click a}] (for [x xs] [b {:on-click c}])]] listed)"
                             "(let [mapped [:div [b {:on-click a}] (map (fn [x] (prn x) [b {:on-click c}]) xs)]] mapped)"
                             "(let [#_old kept [:div [b {:on-click f}] [b {:on-click g}]]] kept)"
                             "(let [maybe (when ok? [:div [b {:on-click f}] [b {:on-click g}]])] maybe)"
                             "(let [keyed [:ul [b {:on-click a}] (for [i is] ^{:key i} [b {:on-click f}])]] keyed)"
                             "(let [keyed-row ^{:key k} [:div [b {:on-click f}] [b {:on-click g}]]] keyed-row)"
                             "(let [trailing [:div [b {:on-click h}] (for [i is] [b {:on-click f}] #_[b])]] trailing)"]))))

(deftest let-like-look-alikes-are-left-alone
  (is (empty? (inline-components ["(x/let [a [:div [b {:on-click f}] [b {:on-click g}]]] a)"
                                  "(x/with-let [a [:div [b {:on-click f}] [b {:on-click g}]]] a)"
                                  "(let [a [:div [b {:on-click f}] (x/for [i is] [b {:on-click g}])]] a)"
                                  "(let [a (x/when c [:div [b {:on-click f}] [b {:on-click g}]])] a)"]))
      "skips bindings and loops of heads resolving outside core and reagent"))

(deftest let-binding-without-several-handler-elements-is-left-alone
  (is (empty? (inline-components ["(let [launcher [button {:on-click run} \"Run\"]] (if busy? [tooltip launcher] launcher))"
                                  "(let [title [:h2 {:class (heading)} label]] title)"
                                  "(let [cards [:ul (for [item items] [card {:on-open f}])]] cards)"
                                  "(let [hover [:div {:on-mouse-enter a :on-mouse-leave b}]] hover)"
                                  "(let [save [button {:on-click #(dispatch [::open {:on-confirm f}])} \"Save\"]] save)"
                                  "(let [event [::open {:on-success f} [b {:on-click g}]]] event)"
                                  "(let [{:keys [a]} [:div [b {:on-click f}] [b {:on-click g}]]] a)"
                                  "(let [x (render [b {:on-click f}] [b {:on-click g}])] x)"
                                  "(let [x [:div :data [b {:on-click f}] [b {:on-click g}]]] x)"
                                  "(let [x [:div [toggle {:on-label \"On\"}] [toggle {:on-label \"Yes\"}]]] x)"
                                  "(let [x [:div [b {:foo/on-thing f}] [b {:foo/on-other g}]]] x)"
                                  "(let [x [:div [b {:render f}] [b {:handler g}]]] x)"
                                  "(let [x [:div [b {:on-label \"Line\nOne\"}] [b {:on-label \"Line\nTwo\"}]]] x)"
                                  "(let [x [:div (if a [b {:on-click f}] [b {:on-click g}])]] x)"
                                  "(let [x [:div (helper [b {:on-click f}]) [b {:on-click g}]]] x)"
                                  "(let [x [:div #_[b {:on-click f}] [b {:on-click g}]]] x)"
                                  "(when-let [x [:div [b {:on-click f}] [b {:on-click g}]]] x)"
                                  "(binding [x [:div [b {:on-click f}] [b {:on-click g}]]] x)"
                                  "'(let [x [:div [b {:on-click f}] [b {:on-click g}]]] x)"
                                  "#_(let [x [:div [b {:on-click f}] [b {:on-click g}]]] x)"
                                  "(defclass panel [] (let [x [:div [b {:on-click f}] [b {:on-click g}]]] x))"]))
      "one handler element, one per loop or branch, a handler body, data, a destructuring name, or no let"))

(deftest hiccup-before-last-body-form-is-flagged
  (is (= ["[:div \"Shown?\"]"
          "[:p \"Hidden\"]"
          "[:h1 title]"
          "[header]"
          "[nav]"
          "[:a]"
          "[:li x]"
          "[toolbar]"
          "[:dt k]"
          "[ui/panel]"
          "[:<> [row] [cell]]"
          "^{:key 1} [row]"
          "[:a]"
          "[:b]"
          "[:p a]"
          "[:p]"
          "[:p]"
          "[:p]"
          "[:p]"
          "[:p]"
          "[:p]"
          "[:p]"
          "[:p]"
          "[:em]"]
         (discarded-hiccup ["(when open? [:div \"Shown?\"] [:div \"Shown!\"])"
                            "(when-not open? [:p \"Hidden\"] [:p \"Shown\"])"
                            "(defn view \"Doc.\" [title] [:h1 title] [:p title])"
                            "(defn view [] [header] [body])"
                            "(defn- view [] [nav] [body])"
                            "(defn view ([] [:a] [:b]) ([x] [:c x]))"
                            "(fn [x] [:li x] [:li x])"
                            "(rum/defc bar [] [toolbar] [content])"
                            "(defmethod row [:term :def] [[k v]] [:dt k] [:dd v])"
                            "(let [a 1] [ui/panel] [ui/footer])"
                            "(do [:<> [row] [cell]] [footer])"
                            "(when x ^{:key 1} [row] [row])"
                            "(do [:a] [:b] [:c])"
                            "(r/with-let [a 1] [:p a] [:span a] (finally (stop)))"
                            "(when-let [a b] [:p] [:span])"
                            "(when-some [a b] [:p] [:span])"
                            "(when-first [a b] [:p] [:span])"
                            "(letfn [(f [] 1)] [:p] [:span])"
                            "(loop [a 1] [:p] [:span])"
                            "(binding [*a* 1] [:p] [:span])"
                            "(with-redefs [a 1] [:p] [:span])"
                            "(when #_skipped x [:p] [:span])"
                            "(defn view [x] (when x [:em] [:strong]))"]))))

(deftest hiccup-that-is-returned-or-not-in-a-body-is-left-alone
  (is (empty? (discarded-hiccup ["(when x [:div])"
                                 "(when x (prn x) [:div])"
                                 "(when [:a] [:b])"
                                 "(let [a [:div] b [:span]] [:p a b])"
                                 "(do [:db :path] [:x])"
                                 "(do [:dispatch x] [:x])"
                                 "(do [1 2] x)"
                                 "(if x [:a] [:b])"
                                 "(cond x [:a] :else [:b])"
                                 "(defn view [x] {:pre [(pos? x)]} [:div x])"
                                 "(defmethod row [:term :def] [[k v]] [:dt k])"
                                 "(defmethod view [:a :b] [item] [:div item])"
                                 "(r/with-let [a 1] [:p a] (finally (stop)))"
                                 "(when x [:a] #_[:b])"
                                 "(when x #_[:a] [:b])"
                                 "(when #_skipped [:a] [:b])"
                                 "(defn view [] [:a] #_[:b])"
                                 "(defn view ([] [:a]) {:doc [:p] :added \"1.0\"})"
                                 "(fn* [x] [:li x] [:li x])"
                                 "(other [:a] [:b])"
                                 "(s/defn names :- [s/Str] [users] (map :name users))"
                                 "(s/fn :- [s/Int] [xs] (map inc xs))"
                                 "(s/defmethod render :k :- [s/Str] [x] (str x))"]))))

(defn- file-discarded-hiccup
  "Analyze `source` as the only file of a project, and return the forms discarded-hiccup flags."
  [source]
  (->> (temp-project/run-group reagent/group {"core.cljs" source}) :results :discarded-hiccup (mapv :form)))

(defn- redundant-intos [lines] (list-findings :redundant-into-hiccup :form library-aliases lines))

(deftest into-from-keyed-core-mapping-is-flagged
  (is (= ["(into [:ul] (for [i is] ^{:key i} [:li i]))"
          "(into [:ul] (map (fn [i] (with-meta [:li i] {:key i})) is))"
          "(into [:ul] (keep-indexed (fn [n i] ^{:key n} [:li i]) is))"]
         (redundant-intos ["(into [:ul] (for [i is] ^{:key i} [:li i]))"
                           "(into [:ul] (map (fn [i] (with-meta [:li i] {:key i})) is))"
                           "(into [:ul] (keep-indexed (fn [n i] ^{:key n} [:li i]) is))"]))
      "reads core into, for, map, keep-indexed, fn and with-meta"))

(deftest into-inside-non-hiccup-forms-is-left-alone
  (is (empty? (redundant-intos ["'(into [:ul] (for [i is] ^{:key i} [:li i]))"
                                "(defclass c [] (into [:ul] (for [i is] ^{:key i} [:li i])))"
                                "(ns a (:require (into [:ul] (for [i is] ^{:key i} [:li i]))))"]))
      "skips quoted, style-declaration and ns forms"))

(deftest into-through-look-alike-heads-is-left-alone
  (is (empty? (redundant-intos ["(x/into [:ul] (for [i is] ^{:key i} [:li i]))"
                                "(into [:ul] (x/for [i is] ^{:key i} [:li i]))"
                                "(into [:ul] (x/map (fn [i] ^{:key i} [:li i]) is))"
                                "(into [:ul] (map (x/fn [i] ^{:key i} [:li i]) is))"
                                "(into [:ul] (map (fn [i] (x/with-meta [:li i] {:key i})) is))"
                                "(let [into f] (into [:ul] (for [i is] ^{:key i} [:li i])))"
                                "(into [:ul] (for [i is] (x/when i ^{:key i} [:li i])))"
                                "(into [:ul] (for [i is] (if c (x/when c [:li i]) ^{:key i} [:li i])))"
                                "(into [:ul] (for [i is] (cond c (x/if c [:li i] [:li]) :else ^{:key i} [:li i])))"
                                "(into [:ul] (for [i is] (let [when f] (if c (when c [:li i]) ^{:key i} [:li i]))))"]))
      "skips heads resolving outside core"))

(deftest heads-referred-as-macros-are-read
  (is (= ["[:p]"]
         (file-discarded-hiccup "(ns app.core (:require-macros [reagent.core :refer [with-let]]))
(with-let [a 1] [:p] [:span])"))
      "reads head referred through require-macros")
  (is (= ["[:p]"]
         (file-discarded-hiccup "(ns app.core (:require [rum.core :refer-macros [defc]]))
(defc view [] [:p] [:span])"))
      "reads head referred through refer-macros"))

(deftest heads-resolving-outside-core-reagent-and-rum-are-left-alone
  (is (empty? (discarded-hiccup {:refers {"defn" "schema.core"}}
                                ["(defn names :- [s/Str] [people] (map :name people))"]))
      "skips a head referred from another library")
  (is (empty? (discarded-hiccup {:shadowed-core #{"fn"}}
                                ["(fn :- [s/Int] [xs] (map inc xs))"]))
      "skips a head the ns form excludes from core")
  (is (empty? (discarded-hiccup {:shadowed-core :all} ["(when x [:p] [:span])"]))
      "skips every core head when the ns form may shadow any")
  (is (empty? (discarded-hiccup ["(x/defc bar [] [toolbar] [content])"
                                 "(defc bar [] [toolbar] [content])"
                                 "(x/with-let [a 1] [:p] [:span])"
                                 "(x/when a [:p] [:span])"]))
      "skips a head from an unknown library")
  (is (= ["[:p]" "[:em]" "[:b]" "[:i]"]
         (discarded-hiccup {:refers {"with-let" "reagent.core"}
                            :shadowed-core #{"defn"}}
                           ["(when x [:p] [:span])"
                            "(with-let [a 1] [:em] [:strong])"
                            "(clojure.core/when x [:b] [:span])"
                            "(cljs.core/when x [:i] [:span])"]))
      "reads heads resolving to core, Reagent or Rum"))

(deftest data-def-discarded-hiccup-is-left-alone
  (is (empty? (discarded-hiccup ["(def s [:vector {:x (do [:my-a :b] :int)} :int])"
                                 "(def s [:vector {:x (do [:string {:min 1}] :int)} :int])"]))
      "vectors dropped in data def value are data"))

(deftest unrendered-discarded-hiccup-is-left-alone
  (is (empty? (discarded-hiccup ["'(do [:a] [:b])"
                                 "#_(do [:a] [:b])"
                                 "`(do [:a] [:b])"
                                 "(defclass panel [] (do [:a] [:b]))"
                                 "(comment (defn f [] [:div] [:span]))"]))))

(defn- remounting-branches [lines] (list-findings :remounting-branch :hint library-aliases lines))

(defn- wrapped-hint
  ([child wrapper] (wrapped-hint child wrapper (str "Render " wrapper " in both branches.")))
  ([child wrapper fix]
   (str child " renders inside " wrapper " in one branch and bare in the other, so React mounts it again when the"
        " condition flips, and keyboard focus inside it falls to the page. " fix)))

(deftest wrapper-around-focusable-child-in-one-branch-is-flagged
  (is (= [(wrapped-hint "launcher" "blocked-action")
          (wrapped-hint "checkbox" "tooltip")
          (wrapped-hint "[:button \"Go\"]" "tooltip")
          (wrapped-hint "[row {:on-click f}]" ":span")
          (wrapped-hint "[:div [:a {:href h} \"Open\"]]" ":li")
          (wrapped-hint "[:span {:tab-index 0} t]" "tooltip")
          (wrapped-hint "[:div (when y [:button \"Go\"])]" "tooltip")
          (wrapped-hint "b" "tooltip")
          (wrapped-hint "[:select]" "tooltip" "Render the same elements around it, in the same order, in both branches.")
          (wrapped-hint "b" "tooltip")
          (wrapped-hint "b" "tooltip" "Render the same elements around it, in the same order, in both branches.")
          (wrapped-hint "(when y [:input])" ":span")
          (wrapped-hint "[:input]" ":li")
          (wrapped-hint "[:input]" ":li")]
         (remounting-branches
          [(str "(defn a [b r] (let [launcher [button {:on-click go} \"Go\"]]"
                " (if b [blocked-action {:reason r} launcher] launcher)))")
           (str "(defn c [d] (let [checkbox [:input {:type \"checkbox\"}]]"
                " [:div (if d [tooltip {:content \"x\"} checkbox] checkbox)]))")
           "(defn e [d] (if-not d [:button \"Go\"] [tooltip [:button \"Go\"]]))"
           "(defn f [w f] (if-let [y w] [:span {:class y} [row {:on-click f}]] [row {:on-click f}]))"
           "(defn g [w h] (if w [:li [:div [:a {:href h} \"Open\"]]] [:div [:a {:href h} \"Open\"]]))"
           "(defn i [w t] (if-some [_ w] [tooltip [:span {:tab-index 0} t]] [:span {:tab-index 0} t]))"
           "(defn k [x y] (if x [tooltip [:div (when y [:button \"Go\"])]] [:div (when y [:button \"Go\"])]))"
           "(defn l [x y] (let [b (when y [:button \"Go\"])] (if x [tooltip b] b)))"
           "(defn m [x] (if x [tooltip [:b \"Hint\"] [:select]] [:select]))"
           "(defn n [x opts] (let [b [:button \"Go\"]] (if x [tooltip opts b] b)))"
           "(defn o [x icon] (let [b [:button \"Go\"]] (if x [tooltip {:a 1} icon b] b)))"
           "(defn p [x y] (if x [:span (when y [:input])] (when y [:input])))"
           "(defn q [x xs] [:ul (map (fn [i] (if x [:li [:input]] [:input])) xs)])"
           "(defn r [x] (r/create-class {:reagent-render (fn [] (if x [:li [:input]] [:input]))}))"]))))

(deftest remounting-branch-is-keyed-by-wrapped-child
  (is (= ["[row {:on-click f}]"]
         (list-findings :remounting-branch :inner-form library-aliases
                        ["(defn a [w f] (if w [:span [row {:on-click f}]] [row {:on-click f}]))"]))))

(deftest branches-that-keep-focus-are-left-alone
  (is (empty? (remounting-branches
               ["(defn a [b r launcher] (if b [blocked-action {:reason r} launcher] launcher))"
                "(defn c [n] (let [label (str n)] (if n [button {} label] label)))"
                "(defn d [x] (let [t [:b \"t\"]] (if x [:span t] t)))"
                "(defn e [x] (if x [:span \"Saved\"] \"Saved\"))"
                "(defn f [loading? rows] (if loading? [spinner] [table rows]))"
                "(defn g [x a b face] (if x [:button {:class a} face] [:button {:class b} face]))"
                "(defn h [x] [:div (if x [:<> [:input]] [:input])])"
                "(defn i [x] (when x [tooltip [:input]]))"
                "(defn j [x] (if x [:span [:input]] [:div [:input]]))"
                "(defn k [x] (if x (wrap [:input]) [:input]))"
                "(def l (if x [:span [:input]] [:input]))"
                "(defn m [] (let [c [:input]] (if c [:span c])))"
                "(defn n [x f l] (if x [tooltip [:span {:on-click (fn [e] (f e))} l]] [:span {:on-click (fn [e] (f e))} l]))"
                "(defn o [x a c] (if x [:strong [:span (str/join \" \" [a c])]] [:span (str/join \" \" [a c])]))"
                "(defn p [x items] (let [btn [:button \"Go\"]] (for [btn items] (if x [:li btn] btn))))"
                "(defn q [x] (if x [tooltip [:a \"Open\"]] [:a \"Open\"]))"
                "(defn r [x v] (if x [:label [:input {:type \"hidden\" :value v}]] [:input {:type \"hidden\" :value v}]))"
                "(defn s [x f] (if x [:a {:href \"/\"} \"Open\"] [:button {:on-click f} \"Open\"]))"
                "(defn t [x t] (if x [:span [highlight {:text t}]] [highlight {:text t}]))"
                "(defn u [x] (if x [:span [:button {:disabled true} \"Go\"]] [:button {:disabled true} \"Go\"]))"
                "(defn v [x attrs] (if x [tooltip [:button attrs \"Go\"]] [:button attrs \"Go\"]))"
                (str "(defn w [x f] (if x [tooltip [:div {:role \"button\" :on-click f} \"Go\"]]"
                     " [:div {:role \"button\" :on-click f} \"Go\"]))")
                "(defn y [x] (let [b [:button]] (if-let [b (f)] [:span b] b)))"
                "(defn z [x] [:div {:on-click (fn [] (if x [:span [:input]] [:input]))}])"
                "(defn z2 [x xs] (filter (fn [i] (if x [:span [:input]] [:input])) xs))"]))
      (str "child that holds no focus, a parameter, a fragment, unrelated branches, a tag change, one branch, no"
           " rendering, props and call arguments, a name bound again, a component without a handler, a disabled or"
           " hidden control, unreadable props, a role without a tabindex"))
  (is (empty? (remounting-branches ["'(defn a [x] (if x [:span [:input]] [:input]))"
                                    "#_(defn a [x] (if x [:span [:input]] [:input]))"
                                    "(comment (defn a [x] (if x [:span [:input]] [:input])))"
                                    "(defclass panel [] (if x [:span [:input]] [:input]))"]))
      "unrendered code"))
