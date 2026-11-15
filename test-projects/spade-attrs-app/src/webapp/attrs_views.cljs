(ns webapp.attrs-views
  (:require
   [webapp.attrs-styles :as styles]))

(defn card [props] [:div props])

(defn flagged-views [props id color cache]
  [:div
   [:div (merge (styles/merge-attrs) props)]
   [:div (assoc (styles/assoc-attrs) :id id)]
   [:div (assoc-in (styles/assoc-in-attrs) [:style :color] color)]
   [:div (update (styles/update-attrs) :class str " active")]
   [:div (dissoc (styles/dissoc-attrs) :style)]
   [:div (conj (styles/conj-attrs) [:id id])]
   [:div (into {:id id} (styles/into-attrs))]
   [:div {:class (:class (styles/keyword-attrs))}]
   [:div {:class (get (styles/get-attrs) :class)}]
   [:div {:style {:color (get-in (styles/get-in-attrs) [:style :color])}}]
   [:div {:class (-> (styles/thread-lookup-attrs) :class)}]
   [:div (-> (styles/thread-assoc-attrs) (assoc :id id))]
   [:div {:class (-> cache (styles/thread-step-attrs) :class)}]
   [:div (-> props (merge (styles/threaded-merge-attrs)))]
   [:div {:class (-> (get (styles/thread-head-attrs) :class) str)}]
   [:div {:class (str "card " (get (styles/class-text-attrs) :class))}]
   (map #(merge (styles/fn-literal-attrs) %) cache)])

(defn repeated-views [props id]
  [:div
   [:div (merge (styles/repeated-attrs) props)]
   [:div (merge props (styles/repeated-attrs))]
   [:div (assoc (styles/repeated-attrs) :id id)]])

(defn wrap-props [props] props)

(defn with-variant [props variant] (assoc props :data-variant variant))

(defn left-alone-views [props cache cached?]
  [:div
   [:div (styles/whole-props-attrs)]
   [card (styles/component-props-attrs)]
   [:div (wrap-props (styles/passed-attrs))]
   [:div (with-variant (styles/variant-attrs) :compact)]
   [card {:attrs (styles/prop-attrs)}]
   [card (assoc props :attrs (styles/value-attrs))]
   [:div {:class (:class props (styles/fallback-attrs))}]
   [:div (-> cache (get (styles/cache-key-attrs)))]
   [:div (some-> cache (get (styles/some-cache-key-attrs)))]
   [:div (cond-> cache cached? (get (styles/cond-cache-key-attrs)))]
   [:div (-> (styles/map-step-key-attrs) {:compact "c"})]
   [:div (-> (styles/lone-thread-attrs))]
   [:div (merge {:class (styles/merged-class)} props)]
   [:div (merge (styles/merged-class) props)]])
