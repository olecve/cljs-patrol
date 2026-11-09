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
                                    :empty-fragment-props []})]
    (is (= 3 (count lines)))
    (is (= 1 (second (first lines))))
    (is (= 0 (second (second lines))))
    (is (= ["Empty fragment props:" 0] (last lines)))))

(defn- empty-fragment-props
  "Walk `lines` the way the parser walks a file, and return the forms the rule reports."
  [lines]
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
         :empty-fragment-props
         (mapv :form))))

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
