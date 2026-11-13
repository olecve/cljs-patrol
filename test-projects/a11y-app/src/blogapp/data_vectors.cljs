(ns blogapp.data-vectors
  (:require [blogapp.ui :as ui :refer [icon-x]]))

(def chip-schema
  [:map
   [:label {:description "Chip label"} [:or :string :int]]
   [:button {:optional true} :boolean]])

(defn ok-chip-shell [{:keys [href on-click]}]
  (if href
    [:a {:href href
         :on-click on-click}]
    [:button {:on-click on-click
              :type "button"}]))

(defn chip [props label]
  (let [[tag attrs] (ok-chip-shell props)]
    [tag attrs label]))

(defn ok-into-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn into-button [on-click label]
  (into (ok-into-shell on-click) [label]))

(defn ok-conj-shell [href]
  [:a {:href href}])

(defn conj-link [href label]
  (conj (ok-conj-shell href) label))

(defn bad-bound-whole-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn bound-whole [on-click]
  (let [shell (bad-bound-whole-shell on-click)]
    shell))

(defn bad-conj-child-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn toolbar [on-click]
  (conj [:div] (bad-conj-child-shell on-click)))

(defn ok-option-renders-local-component [options option-component]
  [:div {:aria-label "Options"
         :role "listbox"}
   (for [option options]
     ^{:key option}
     [:button {:on-click #(prn option)
               :type "button"}
      [option-component {:option option}]])])

(defn bad-button-holding-an-icon-component [on-click]
  [:button {:on-click on-click
            :type "button"}
   [ui/icon-x {}]])

(defn bad-button-holding-a-referred-icon [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-x {}]])

(defn bad-unreturned-button [log on-click]
  (swap! log conj [:button {:on-click on-click
                            :type "button"}])
  [:a {:href "/"}])

(defn unreturned-link [log on-click label]
  (into (bad-unreturned-button log on-click) [label]))
