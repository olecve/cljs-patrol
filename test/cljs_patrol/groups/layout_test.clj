(ns cljs-patrol.groups.layout-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.layout :as layout]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [rewrite-clj.zip :as z]))

(defn- parse
  "Walk `lines` the way the parser walks a file, and return the group's result map."
  ([lines] (parse layout/group lines))
  ([rule-group lines]
   (let [{:keys [handle-list handle-vector]} (group/parse-handlers rule-group)
         declarations (loop [loc (z/of-string (str/join "\n" lines) {:track-position? true})
                             found []]
                        (if (z/end? loc)
                          found
                          (let [handler (case (z/tag loc)
                                          (:list :fn) handle-list
                                          :vector handle-vector
                                          nil)]
                            (recur (z/next loc)
                                   (into found (:decls (some-> handler (apply [loc {} "views.cljs"]))))))))]
     (group/analyze rule-group {:declarations declarations}))))

(defn- rows [result rule]
  (mapv :row (get result rule)))

(deftest staircase-of-sibling-maps-is-flagged
  (let [result (parse [":coordinates [{:x 0"
                       "               :y 0} {:x 10"
                       "                      :y 0} {:x 10"
                       "                             :y 10}]"])]
    (is (= [2 3] (rows result :form-after-split-map))
        "each map starting on the closing line of the one before it")
    (is (empty? (:split-map-after-sibling result))
        "a map already reported as following a split map is not reported twice")))

(deftest maps-on-their-own-lines-are-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse [":coordinates [{:x 0"
                 "               :y 0}"
                 "              {:x 10"
                 "               :y 0}]"]))))

(deftest single-line-sibling-maps-are-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["[{:a 1} {:b 2} {:c 3}]"
                 "(merge {:a 1} {:b 2})"]))))

(deftest call-argument-after-split-map-is-flagged
  (let [result (parse ["(merge {:a 1"
                       "        :b 2} {:c 3"
                       "               :d 4})"
                       "(render-element! {:children [1]"
                       "                  :type \"list\"} {})"])]
    (is (= [2 5] (rows result :form-after-split-map)))
    (is (= ['merge 'render-element!] (mapv :kw (:form-after-split-map result)))
        "the call the arguments belong to")))

(deftest arguments-on-their-own-lines-are-left-alone
  (is (empty? (:form-after-split-map
               (parse ["(render-element! {:children [1]"
                       "                  :type \"list\"}"
                       "                 {})"])))))

(deftest hiccup-child-after-split-props-is-flagged
  (let [result (parse ["[button {:size :s"
                       "         :on-click on-click} [icon {:name :plus"
                       "                                    :size :s}]]"])]
    (is (= [2] (rows result :form-after-split-map)))
    (is (= [30] (mapv :col (:form-after-split-map result)))
        "the column of the child, where the fix starts")))

(deftest short-string-hiccup-child-is-left-alone
  (is (empty? (:form-after-split-map
               (parse ["[button {:size :s"
                       "         :on-click on-click} \"Edit\"]"])))))

(deftest lone-symbol-hiccup-child-is-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["[button {:size :s"
                 "         :type :tertiary} items-per-page]"
                 "[:map"
                 " [:page-size {:min 25"
                 "              :max 500} int?]]"]))))

(deftest symbol-child-followed-on-its-line-is-flagged
  (is (= [2] (rows (parse ["[button {:size :s"
                           "         :type :tertiary} label [icon]]"])
                   :form-after-split-map))
      "the symbol is not alone on the closing line"))

(deftest collection-child-after-split-props-is-flagged
  (is (= [2 4] (rows (parse ["[button {:size :s"
                             "         :type :tertiary} {:a 1}]"
                             "[button {:size :s"
                             "         :type :tertiary} [icon]]"])
                     :form-after-split-map))
      "a map or vector child is not a label, however short"))

(deftest multi-line-form-after-closing-line-is-flagged
  (let [result (parse ["[:div"
                       " [button {:size :s"
                       "          :type :tertiary}] [icon {:name :plus"
                       "                                   :size :s}]]"])]
    (is (= [3] (rows result :form-after-split-map))
        "the element ends on the line its props map closes, and the next one starts there")))

(deftest single-line-form-after-nested-split-map-is-left-alone
  (is (empty? (:form-after-split-map
               (parse ["(fn [{:keys [a]"
                       "      :as b}] a)"])))))

(deftest token-after-split-map-in-parameter-list-is-left-alone
  (is (empty? (:form-after-split-map
               (parse ["(fn [_ [_ {:keys [on-success]"
                       "           :as fetch} response]] response)"])))))

(deftest binding-pair-is-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["(let [{:keys [a]"
                 "       :as b} (init)"
                 "      [x y] {:a 1"
                 "             :b 2}]"
                 "  a)"
                 "(for [x xs"
                 "      :let [{:keys [c]} {:c 1"
                 "                         :d 2}]]"
                 "  c)"]))))

(deftest cond-and-case-clauses-are-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["(cond {:a 1"
                 "       :b 2} x"
                 "      :else nil)"
                 "(case k"
                 "  [:a :b] {:x 1"
                 "           :y 2})"]))))

(deftest split-map-after-map-or-vector-sibling-is-flagged
  (let [result (parse ["(fn [{:keys [db]} {:keys [id]"
                       "                   :as profile}] db)"
                       "(write-source-note {:db db} {:draft-id draft-id"
                       "                             :source source})"])]
    (is (= [1 3] (rows result :split-map-after-sibling)))
    (is (empty? (:form-after-split-map result)))))

(deftest split-map-after-head-is-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["(f {:a 1"
                 "    :b 2})"
                 "[button {:size :s"
                 "         :on-click f}]"]))))

(deftest split-map-after-symbol-is-left-alone-by-default
  (is (empty? (:split-map-after-sibling
               (parse ["(fn [cofx [_ {:keys [id]"
                       "              :as user-group}]] cofx)"])))))

(deftest split-map-after-symbol-is-flagged-when-configured
  (let [rule-group (layout/make-group {:split-map-after-symbol true})]
    (is (= [1] (rows (parse rule-group ["(fn [cofx [_ {:keys [id]"
                                        "              :as user-group}]] cofx)"])
                     :split-map-after-sibling)))
    (is (empty? (:split-map-after-sibling
                 (parse rule-group ["(reset! state {:a 1"
                                    "               :b 2})"])))
        "a symbol outside a binding form is the target of a call, configured or not")))

(deftest split-map-after-symbol-in-method-parameters-is-flagged-when-configured
  (let [rule-group (layout/make-group {:split-map-after-symbol true})]
    (doseq [[lines description]
            [[["(defmethod handle [:a :b] [db {:keys [id]"
               "                               :as event}] db)"]
              "a defmethod's parameters follow a dispatch value that is a vector itself"]
             [["(letfn [(helper [this {:keys [a]"
               "                       :as m}] a)] helper)"]
              "a letfn spec is a fn without a fn head"]
             [["(defrecord R [] P (method [this {:keys [a]"
               "                                 :as m}] a))"]
              "a protocol method is a fn without a fn head"]]]
      (is (= [1] (rows (parse rule-group lines) :split-map-after-sibling))
          description))
    (is (empty? (:split-map-after-sibling
                 (parse rule-group ["(letfn [] [(render [_ {:keys [a]"
                                    "                      :as m}])])"])))
        "only letfn's first argument holds fn specs, so a call in its body is not one")))

(deftest structure-before-split-map-is-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["(fn [i c] {:space-before? (zero? i)"
                 "           :text c})"
                 "(let [x 1] {:a x"
                 "            :b 2})"
                 "(assoc-in db [:a :b] {:x 1"
                 "                      :y 2})"]))))

(deftest braces-in-strings-comments-and-regexes-are-left-alone
  (is (= {:form-after-split-map []
          :split-map-after-sibling []}
         (parse ["(str \"{:a 1\""
                 "     \":b 2} {:c 3\")"
                 "(f x) ; {:a 1} {:b"
                 "(re-find #\"\\{a\" s)"]))))

(deftest identity-survives-reformatting
  (let [finding (first (:form-after-split-map
                        (parse ["(merge {:a 1"
                                "        :b 2} {:c 3})"])))
        identity-of (fn [row] (baseline/issue->identity :form-after-split-map (assoc finding :row row)))]
    (is (= (identity-of 2) (identity-of 40))
        "the row moves whenever anything above it does")
    (is (= #{:rule :kind :file :form} (set (keys (identity-of 2)))))))

(deftest identity-reads-back-from-edn
  (let [finding (first (:form-after-split-map
                        (parse ["[{:x 0"
                                "  :y 0} {:x 10}]"])))
        identity (baseline/issue->identity :form-after-split-map finding)]
    (is (= identity (edn/read-string (pr-str identity)))
        "the baseline file stores it as EDN, and a kind printing as [] would read back as a vector")))

(deftest identity-separates-same-form-in-different-definitions
  (let [found (:form-after-split-map
               (parse ["(defn saved [db]"
                       "  (update-op db {:id id"
                       "                 :source source} #{:loading}))"
                       "(reg-event-fx ::failed"
                       "  (fn [db] (update-op db {:id id"
                       "                          :source source} #{:loading})))"]))
        identities (map #(baseline/issue->identity :form-after-split-map %) found)]
    (is (= ["saved" "::failed"] (map :test identities))
        "the var a def names, or the keyword a registration is keyed by")
    (is (= 2 (count (set identities)))
        "the same call in two handlers of one file stays two baseline entries")))
