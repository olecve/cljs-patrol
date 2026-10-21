(ns webapp.helpers-test
  (:require
   [react-testing-library-cljs.screen :as s :refer [query-by-text]]
   [react-testing-library-cljs.within :as within]
   [webapp.other :as other]))

(defn master-prompt []
  (s/get-by-role "textbox" {:name "Master prompt"}))

(defn- query-save-button []
  (s/get-by-role "button" {:name "Save"}))

(defn get-toast []
  (s/query-by-text "Saved"))

(defn loaded-panel []
  (s/find-by-role "region"))

(defn rows []
  (s/get-all-by-role "row"))

(defn referred-hint []
  (query-by-text "Hint"))

(defn radio-in-group [group]
  (within/get-by-role (s/get-by-role "radiogroup" {:name group}) "radio"))

(defn ^:private documented-button
  "The save button."
  []
  (s/get-by-role "button"))

(defn get-name-input []
  (s/get-by-role "textbox" {:name "Name"}))

(defn query-ai-working []
  (s/query-by-text "AI is working"))

(defn find-dialog []
  (s/find-by-role "dialog"))

(defn get-rows []
  (s/get-all-by-role "row"))

(defn query-all-buttons []
  (s/query-all-by-role "button"))

(defn button-text []
  (.-textContent (s/get-by-role "button")))

(defn row-of [label]
  (other/row-of (s/get-by-role "button" {:name label})))

(defn click-save []
  (s/get-by-role "button" {:name "Save"})
  (other/click!))

(defn other-lookup []
  (other/get-by-role "button"))

(defn shadowed [query-by-text]
  (query-by-text "Hint"))

(defmethod other/lookup :button [_]
  (s/get-by-role "button"))

(defn multi-arity
  ([] (s/get-by-role "button"))
  ([name] (s/get-by-role "button" {:name name})))

(def save-button-fn (fn [] (s/get-by-role "button")))

(defn get-spinner []
  (s/find-by-role "progressbar"))

(defn query-menu []
  (s/find-by-role "menu"))

(defn find-all-items []
  (s/find-all-by-role "listitem"))

(defn hinted-params ^js []
  (s/get-by-role "banner"))

(defn with-discard []
  #_(prn "debug")
  (s/get-by-role "contentinfo"))

(defn ^:private with-attr-map
  "The navigation,
  as rendered."
  {:added "1"}
  []
  (s/get-by-role "navigation"))

(defn with-pre []
  {:pre [true]}
  (s/get-by-role "main"))

(defn acted []
  (other/act #(s/get-by-role "button")))

(defn threaded []
  (-> (s/get-by-role "button") .-parentElement))

(defn let-shadowed []
  (let [query-by-text (fn [_] nil)]
    (query-by-text "Hint")))

(defn redefined []
  (with-redefs [query-by-text (fn [_] nil)]
    (query-by-text "Hint")))

(comment
  (defn in-comment []
    (s/get-by-role "button")))

#_(defn discarded []
    (s/get-by-role "button"))

'(defn quoted []
   (s/get-by-role "button"))
