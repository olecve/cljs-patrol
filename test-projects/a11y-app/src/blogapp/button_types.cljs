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
