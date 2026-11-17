(ns blogapp.button-types
  (:require [blogapp.ui :as ui]))

(defn no-props [] [:button "Save"])

(defn no-type [save!] [:button {:on-click save!} "Save"])

(defn class-shorthand [save!] [:button.primary {:on-click save!} "Save"])

(defn invalid-string [] [:button {:type "btn"} "Save"])

(defn invalid-keyword [] [:button {:type :btn} "Save"])

(defn built-invalid [props] [:button (assoc props :type "btn") "Save"])

(defn typed-button [] [:button {:type "button"} "Save"])

(defn submit-button [] [:button {:type "submit"} "Send"])

(defn reset-button [] [:button {:type :reset} "Clear"])

(defn symbol-type [kind] [:button {:type kind} "Save"])

(defn opaque-props [props] [:button props "Save"])

(defn call-props [props] [:button (merge-props props) "Save"])

(defn built-unknown [props] [:button (assoc props :on-click identity) "Save"])

(defn merged-type [props] [:button (merge {:type "btn"} props) "Save"])

(defn component-button [] [ui/button {:on-click identity} "Save"])

(defn not-a-button [] [:div {:role "button" :tab-index 0} "Save"])

(defn bare-button [] [:button])

(defn component-styling-type [] [ui/button {:type :primary} "Save"])

(defn component-html-type [] [ui/button {:type :submit} "Send"])

(defn component-built-styling-type [props] [ui/button (assoc props :type :secondary) "Save"])

(defn text-input [] [:input {:type "text"}])

(defn data-path [theme] (get-in theme [:button :primary :bg]))

(defn nil-type [] [:button {:type nil} "Save"])

(defn false-type [] [:button {:type false} "Save"])

(defn upper-case-type [] [:button {:type "Button"} "Save"])

(defn upper-case-keyword-type [] [:button {:type :Submit} "Send"])

(defn discarded-before-props [props] [:button #_{:type "button"} props "Save"])

(defn let-bound-props [] (let [base {:class "btn"}] [:button base "Save"]))

(def base-props {:class "btn"})

(defn def-bound-props [] [:button base-props "Save"])

(defn string-keys [] [:button {"type" "button"} "Save"])

(defn complete-built [] [:button (merge {:class "x"} {:on-click identity}) "Save"])

(defn id-shorthand [] [:button#save {:on-click identity} "Save"])

(defn label-symbol [] (let [label "Save"] [:button label]))

(defn selector [] (css [:button {:color "red"} [:&:hover {:color "blue"}]]))

(defn- button-shell [f] [:button {:on-click f}])

(defn filled-shell [f] (let [[tag attrs] (button-shell f)] [tag (assoc attrs :type "button") "Save"]))

(defn symbol-bound-props [props] (let [p props] [:button p "Save"]))

(defn chained-props [] (let [base {:type "button"} p base] [:button p "Save"]))

(defn discarded-before-map [] [:button #_{:class "x"} {:type "button"} "Save"])

(defn malli-entry [x] (validate [:map [:button {:optional true} :string]] x))

(defn malli-nested [x] (validate [:map [:button [:map [:label :string]]]] x))

(defn plain-selector [] (css [:button {:border "none"}]))

(defn one-key-path [m] (get-in m [:button]))

(defn numeric-type [] [:button {:type 1} "Save"])

(defn symbol-bound-label [props] (let [p props] [:label p "Name"]))

(defn symbol-bound-hidden [props] (let [p props] [:div {:aria-hidden true} [:button p "x"]]))

(defn long-save [f] [:button {:class "toolbar-action toolbar-action--primary toolbar-action--wide toolbar-action--with-icon toolbar-action--in-dialog-footer" :on-click f} "Save"])

(defn long-delete [f] [:button {:class "toolbar-action toolbar-action--primary toolbar-action--wide toolbar-action--with-icon toolbar-action--in-dialog-footer" :on-click f} "Delete"])

(defn number-before-label [] (let [n 3] [:button n "Save"]))

(defn computed-type [submit?] [:button {:type (if submit? "submit" "button")} "Save"])

(defn named-type [t] [:button {:type (name t)} "Save"])

(defn conditional-type [] [:button {:type #?(:cljs "button" :clj "button")} "Save"])

(defn built-computed-type [props s] [:button (assoc props :type (if s "submit" "button")) "Save"])

(defn component-computed-type [submit?] [ui/button {:type (when submit? :submit)} "Send"])

(defn keyword-path-with-text [db] (get-in db [:button :primary "Save"]))

(def nested-path {:path [:button :sizes [:sm :md]]})

(defn string-key-path [db] (let [k "save"] (get-in db [:button k])))

(defn string-assoc-path [db] (assoc-in db [:button "Save"] true))

(defn malli-title [v] (validate [:map [:button {:title "Submit button"} :string]] v))

(def multi-line-label "Save
all")

(defn multi-line-symbol-label [] [:button multi-line-label])

(defn garden-label [] (css [:label {:color "red"} [:&:hover {:color "blue"}]]))

(defn discarded-before-text [] [:textarea #_{:aria-label "Draft"} "draft"])

(defn threaded-path [db] (let [k "primary"] (some-> db (update-in [:button k] inc))))

(defn thread-first-path [db] (-> db (get-in [:button "label"])))

(defn swap-path [state] (let [id "x"] (swap! state assoc-in [:button id] true)))

(defn dispatched-event [] (dispatch [:button "Save"]))

(defn get-key [m] (get m [:button "a"]))

(defn keyed-in-for [xs] (for [x xs] [:button {:key (:id x)} (:name x)]))

(defn id-only [label] [:button {:id "save"} label])

(defn style-only [text] [:button {:style {:color "red"}} text])

(defn label-path [db] (let [lang "en"] (assoc-in db [:label lang] "x")))

(defn symbol-text-area [] (let [t "draft"] [:textarea t]))
