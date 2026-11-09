(ns cljs-patrol.groups.reagent-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.reagent :as reagent]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

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
  (testing "never fails (warning only)"
    (is (not (group/failed? reagent/group {:defclass-as-sole-attr [defclass-sole]})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines reagent/group
                                   {:defclass-as-sole-attr [defclass-sole]
                                    :redundant-into-hiccup []
                                    :empty-fragment-props []
                                    :redundant-fragment []
                                    :inline-component []})]
    (is (= 5 (count lines)))
    (is (= 1 (second (first lines))))
    (is (= 0 (second (second lines))))
    (is (= [["Empty fragment props:" 0] ["Redundant fragment:" 0] ["Inline component:" 0]] (take-last 3 lines)))))

(defn- fragment-findings
  "Walk `lines` the way the parser walks a file, and return the forms `rule` reports."
  [rule lines]
  (let [{:keys [handle-vector]} (group/parse-handlers reagent/group)
        declarations (loop [loc (z/of-string (str/join "\n" lines) {:track-position? true})
                            found []]
                       (if (z/end? loc)
                         found
                         (recur (z/next loc)
                                (if (= :vector (z/tag loc))
                                  (into found (:decls (handle-vector loc {} "views.cljs")))
                                  found))))]
    (->> (group/analyze reagent/group {:declarations declarations
                                       :usages []})
         rule
         (mapv :form))))

(defn- empty-fragment-props [lines] (fragment-findings :empty-fragment-props lines))

(defn- redundant-fragments
  "Return the forms the rule reports for `lines`, each written as the body of a function."
  [lines]
  (fragment-findings :redundant-fragment (map #(str "(defn view [] " % ")") lines)))

(deftest fragment-whose-next-child-is-no-map-is-flagged
  (is (= ["[:<> {} [edit-controls {:editing? false}] [accept-button]]"
          "[:<> {} (when unsaved? [:span \"Unsaved\"])]"
          "[:<> {}]"
          "[:<> {} \"Saved\"]"
          "[:<> {} 3]"
          "[:<> {} :saved]"
          "[:<> {} nil]"
          "[:<> {} \"line one line two\"]"
          "[:<> {} (if ready? [:span \"Ready\"] \"Waiting\")]"
          "[:<> {} (cond saved? [:span] :else nil)]"
          "[:<> {} (case mode :edit [editor] [viewer])]"
          "[:<> {} (let [n 1] [row n])]"
          "[:<> {} #_old [row]]"
          "[:<> #_x {} [row]]"]
         (empty-fragment-props ["[:<> {}\n [edit-controls {:editing? false}]\n [accept-button]]"
                                "[:<> {}\n (when unsaved?\n   [:span \"Unsaved\"])]"
                                "[:<> {}]"
                                "[:<> {} \"Saved\"]"
                                "[:<> {} 3]"
                                "[:<> {} :saved]"
                                "[:<> {} nil]"
                                "[:<> {} \"line one\nline two\"]"
                                "[:<> {} (if ready? [:span \"Ready\"] \"Waiting\")]"
                                "[:<> {} (cond saved? [:span] :else nil)]"
                                "[:<> {} (case mode :edit [editor] [viewer])]"
                                "[:<> {} (let [n 1] [row n])]"
                                "[:<> {} #_old [row]]"
                                "[:<> #_x {} [row]]"]))))

(deftest fragment-whose-next-child-may-be-a-map-is-left-alone
  (is (empty? (empty-fragment-props ["[:<> {} busy-reason [spinner]]"
                                     "[:<> {} (render-children items)]"
                                     "[:<> {} (for [x xs] ^{:key x} [row x])]"
                                     "[:<> {} (when ready? props)]"
                                     "[:<> {} (if ready? [:span] (build-props))]"
                                     "[:<> {} {:a 1}]"
                                     "[:<> {} ^{:key 1} [row]]"
                                     "[:<> {} #?(:cljs [row])]"
                                     "[:<> {} @state]"
                                     "[:<> {} (cond saved? [row] #_#_draft? [draft] :else props)]"
                                     "[:<> {} (case mode #_#_:legacy [old] :edit {:class \"x\"} [viewer])]"
                                     "[:<> {} (case mode #_:legacy :edit {:class \"x\"} [viewer])]"]))))

(deftest fragment-whose-next-child-is-never-nil-is-flagged-before-siblings
  (is (= ["[:<> {} [row] [row]]"
          "[:<> {} \"Saved\" [row]]"
          "[:<> {} (if ready? [:span] \"Waiting\") [row]]"
          "[:<> {} (if-not ready? [:span] [:p]) [row]]"
          "[:<> {} (cond saved? [:span] :else [:p]) [row]]"
          "[:<> {} (cond saved? [:span] true [:p]) [row]]"
          "[:<> {} (case mode :edit [editor] [viewer]) [row]]"
          "[:<> {} (let [n 1] [row n]) [row]]"
          "[:<> {} (do [row]) [row]]"]
         (empty-fragment-props ["[:<> {} [row] [row]]"
                                "[:<> {} \"Saved\" [row]]"
                                "[:<> {} (if ready? [:span] \"Waiting\") [row]]"
                                "[:<> {} (if-not ready? [:span] [:p]) [row]]"
                                "[:<> {} (cond saved? [:span] :else [:p]) [row]]"
                                "[:<> {} (cond saved? [:span] true [:p]) [row]]"
                                "[:<> {} (case mode :edit [editor] [viewer]) [row]]"
                                "[:<> {} (let [n 1] [row n]) [row]]"
                                "[:<> {} (do [row]) [row]]"]))))

(deftest fragment-whose-next-child-may-be-nil-before-siblings-is-left-alone
  (is (empty? (empty-fragment-props ["[:<> {} (when unsaved? [:span]) [row]]"
                                     "[:<> {} (when-not saved? [:span]) [row]]"
                                     "[:<> {} (when-let [x y] [:span x]) [row]]"
                                     "[:<> {} (when-some [x y] [:span x]) [row]]"
                                     "[:<> {} (when-first [x xs] [:span x]) [row]]"
                                     "[:<> {} (if ready? [:span]) [row]]"
                                     "[:<> {} (if ready? [:span] nil) [row]]"
                                     "[:<> {} (if ready? nil [:span]) [row]]"
                                     "[:<> {} (cond saved? [:span]) [row]]"
                                     "[:<> {} (cond saved? [:span] draft? [:p]) [row]]"
                                     "[:<> {} (cond saved? [:span] :else nil) [row]]"
                                     "[:<> {} (cond saved? nil :else [:p]) [row]]"
                                     "[:<> {} (cond saved?) [row]]"
                                     "[:<> {} (case mode :edit [editor]) [row]]"
                                     "[:<> {} (case mode :edit nil [viewer]) [row]]"
                                     "[:<> {} (let [n 1] (when n [row n])) [row]]"
                                     "[:<> {} (let [n 1]) [row]]"
                                     "[:<> {} (do) [row]]"
                                     "[:<> {} nil [row]]"]))))

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
        loc (-> (z/of-string "(defn f []\n  [:<> {} [row]])" {:track-position? true}) z/down z/rightmost)]
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

(deftest unrendered-redundant-fragment-is-left-alone
  (is (empty? (redundant-fragments ["'[:<> [row]]"
                                    "#_[:<> [row]]"
                                    "(defclass panel [] [:<> [:div]])"
                                    "(ns app.views (:require [:<> [row]]))"]))))

(defn- inline-components
  "Walk `lines` the way the parser walks a file, and return the hints the inline-component rule gives."
  [lines]
  (let [handle-lists (:handle-list (group/parse-handlers reagent/group))
        declarations (loop [loc (z/of-string (str/join "\n" lines) {:track-position? true})
                            found []]
                       (if (z/end? loc)
                         found
                         (recur (z/next loc)
                                (if (#{:list :fn} (z/tag loc))
                                  (into found (mapcat #(:decls (% loc {} "views.cljs")) handle-lists))
                                  found))))]
    (->> (group/analyze reagent/group {:declarations declarations
                                       :usages []})
         :inline-component
         (mapv :hint))))

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
