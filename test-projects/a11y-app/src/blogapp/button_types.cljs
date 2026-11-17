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
