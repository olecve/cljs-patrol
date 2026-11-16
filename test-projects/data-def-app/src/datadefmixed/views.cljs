(ns datadefmixed.views
  (:require
   [datadefmixed.ui :as ui]))

(def bad-label-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:label {:title "Name"} :string]])

(def bad-button-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:button {:aria-disabled true} :boolean]])

(def bad-fragment-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:<> {} :int]])

(def bad-save-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:button {:aria-label "Save"
             :data-testid "save-button"} :string]])

(def bad-two-parts-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:label {:title "Name"} [:fn #(string? %)]]])

(def bad-render-schema
  [:map {:render (fn [] [:label {:title "Name"} "x"])}
   [:id :int]])

(def bad-notes-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:textarea {:id "notes-field"} :string]])

(def bad-separator-schema
  [:map {:error/fn (fn [_ _] "Bad")}
   [:hr {:tab-index 0} :int]])

(defn focusable-rule []
  [:hr {:tab-index 0}])

(def bad-alias-schema
  [:map {:render-button [:> ui/button {:on-click identity} "x"]}
   [:label {:title "Name"} :string]])
