(ns cljs-patrol.groups.forbid-props-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.forbid-props :as forbid-props]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.test :refer [deftest is]]))

(def ^:private aria-disabled
  {:prop :aria-disabled
   :disallowed-for #{:button 'app.ui.button/button}
   :message "Use :disabled."})

(def ^:private ns-form
  (str "(ns app.core (:require [app.ui.button :as button :refer [icon-button]]"
       " [app.ui.link :as link] [re-frame.core :as rf]))\n"))

(defn- results [entries source]
  (:results (temp-project/run-group (forbid-props/make-group entries) {"app/core.cljs" (str ns-form source)})))

(defn- flagged
  "Return [prop hint] for each finding."
  ([source] (flagged [aria-disabled] source))
  ([entries source]
   (mapv (juxt :kw :hint) (:forbidden-prop (results entries source)))))

(def ^:private finding [[:aria-disabled "Use :disabled."]])

(deftest native-button-is-flagged
  (doseq [source ["(defn v [ok?] [:button {:aria-disabled (not ok?) :type \"button\"} \"Save\"])"
                  "(defn v [] [:button.primary#save {:aria-disabled true} \"Save\"])"
                  "(defn v [] [:button {:aria-disabled \"true\"} \"Save\"])"]]
    (is (= finding (flagged source))
        source)))

(deftest configured-component-is-flagged
  (doseq [source ["(defn v [] [button/button {:aria-disabled true} \"Comment\"])"
                  "(defn v [] [app.ui.button/button {:aria-disabled true} \"Comment\"])"]]
    (is (= finding (flagged source))
        source))
  (is (= finding (flagged [(assoc aria-disabled :disallowed-for #{'app.ui.button/icon-button})]
                          "(defn v [] [icon-button {:aria-disabled true}])"))
      "referred component"))

(deftest other-elements-are-left-alone
  (doseq [source ["(defn v [] [link/link {:aria-disabled true} \"Docs\"])"
                  "(defn v [] [:a {:aria-disabled true :role \"link\"} \"Docs\"])"
                  "(defn v [d] [:div {:aria-disabled (when d \"true\") :role \"option\"} \"A\"])"
                  "(defn v [] (let [button (fn [])] [button {:aria-disabled true}]))"
                  "(defn v [] [:span {:aria-disabled true} [:button {:disabled true} \"Go\"]])"]]
    (is (= [] (flagged source))
        source)))

(deftest prop-reagent-drops-is-left-alone
  (doseq [value ["false" "nil" "#?(:clj nil :cljs false)"]]
    (is (= [] (flagged (str "(defn v [] [:button {:aria-disabled " value "} \"Save\"])")))
        value)))

(deftest props-built-on-unseen-map-are-left-alone
  (doseq [props ["(merge attrs {:aria-disabled true})" "(assoc attrs :aria-disabled true)" "attrs"]]
    (is (= [] (flagged (str "(defn v [attrs] [:button " props " \"Save\"])")))
        props)))

(deftest props-built-from-readable-parts-are-read
  (doseq [props ["(assoc {:type \"button\"} :aria-disabled true)" "(merge {:type \"button\"} {:aria-disabled true})"]]
    (is (= finding (flagged (str "(defn v [] [:button " props " \"Save\"])")))
        props)))

(deftest props-named-by-symbol-are-read
  (is (= finding (flagged "(def attrs {:aria-disabled true})\n(defn v [] [:button attrs \"Save\"])"))
      "file def holding literal map"))

(deftest re-frame-vectors-are-left-alone
  (doseq [source ["(defn v [] (rf/dispatch [:button {:aria-disabled true}]))"
                  "(defn v [] (rf/dispatch-sync [:button {:aria-disabled true}]))"
                  "(defn v [] @(rf/subscribe [:button {:aria-disabled true}]))"
                  "(rf/reg-sub :x :<- [:button {:aria-disabled true}] identity)"
                  "(rf/reg-event-fx :x (fn [] {:dispatch [:button {:aria-disabled true}]}))"
                  "(rf/reg-event-fx :x (fn [] {:dispatch-n [[:button {:aria-disabled true}]]}))"
                  "(rf/reg-event-fx :x (fn [] {:fx [[:dispatch [:button {:aria-disabled true}]]]}))"]]
    (is (= [] (flagged source))
        source))
  (is (= finding (flagged "(defn v [] [:div {:on-click #(rf/dispatch [:save])} [:button {:aria-disabled true}]])"))
      "element beside dispatch")
  (is (= finding (flagged "(defn v [] [tooltip {:content [:button {:aria-disabled true}]}])"))
      "element as map value under other key"))

(deftest local-shadowing-referred-component-is-left-alone
  (let [entries [(assoc aria-disabled :disallowed-for #{'app.ui.button/icon-button})]]
    (doseq [source ["(defn v [icon-button] [icon-button {:aria-disabled true}])"
                    "(defn v [] (let [icon-button (fn [])] [icon-button {:aria-disabled true}]))"]]
      (is (= [] (flagged entries source))
          source))))

(deftest vectors-that-are-not-elements-are-left-alone
  (is (= [] (flagged [:closed] "(def schema [:map {:closed true} [:id :int]])"))
      "malli schema")
  (is (= [] (flagged [:id] "(defn v [] (rf/dispatch [::save {:id 1}]))"))
      "event vector")
  (is (= [] (flagged "(defn v [] '[:button {:aria-disabled true}])"))
      "quoted vector"))

(deftest allowed-for-forbids-everywhere-else
  (let [entries [{:prop :style
                  :allowed-for #{:div 'app.ui.button/button}}]]
    (is (= [[:style ":style is forbidden on :span."]] (flagged entries "(defn v [] [:span {:style {:color \"red\"}}])"))
        "other tag")
    (is (= [] (flagged entries "(defn v [] [:div {:style {:color \"red\"}}])"))
        "allowed tag")
    (is (= [] (flagged entries "(defn v [] [button/button {:style {:color \"red\"}}])"))
        "allowed component")))

(deftest bare-prop-is-forbidden-everywhere
  (is (= [[:data-test ":data-test is forbidden on app.ui.button/button."]]
         (flagged [:data-test] "(defn v [] [button/button {:data-test \"save\"}])"))
      "component")
  (is (= [[:data-test ":data-test is forbidden on :li."]] (flagged [:data-test] "(defn v [] [:li {:data-test \"row\"}])"))
      "tag"))

(deftest every-forbidden-prop-is-reported
  (is (= #{:aria-disabled :data-test}
         (set (map first (flagged [aria-disabled :data-test] "(defn v [] [:button {:aria-disabled true :data-test \"x\"}])"))))
      "two entries on one element"))

(deftest malformed-config-is-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":forbid-props must be a vector of entries"
                        (forbid-props/make-group {:prop :x}))
      "map instead of vector")
  (doseq [entries [["aria-disabled"]
                   [{:prop "aria-disabled"}]
                   [{:prop :x
                     :allowed-for #{:a}
                     :disallowed-for #{:b}}]
                   [{:prop :x
                     :disallowed-for #{'button}}]
                   [{:prop :x
                     :disallowed-for :button}]
                   [{:prop :x
                     :message :use-disabled}]
                   [{:prop :x
                     :disalowed-for #{:button}}]
                   [{:prop :x
                     :disallowed-for #{:button.primary}}]
                   [{:prop :x
                     :disallowed-for #{:ns/button}}]
                   [:x
                    {:prop :x
                     :disallowed-for #{:button}}]]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"^:forbid-props "
                          (forbid-props/make-group entries))
        (pr-str entries))))

(deftest unconfigured-group-does-nothing
  (is (= {} (group/parse-handlers forbid-props/group))
      "no handlers")
  (is (empty? (group/summary-lines forbid-props/group {}))
      "no summary lines")
  (is (= 1 (count (group/summary-lines (forbid-props/make-group [:x]) {})))
      "summary line once configured"))

(deftest findings-fail-run
  (is (group/failed? forbid-props/group {:forbidden-prop [{:kw :x}]})
      "finding")
  (is (not (group/failed? forbid-props/group {:forbidden-prop []}))
      "no finding"))
