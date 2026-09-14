(ns blogapp.lists
  (:require
   [blogapp.icons :as icons]
   [blogapp.ui :as ui]))

(defn bad-button-in-for [posts]
  (for [post posts]
    [:button {:aria-label "Remove post"
              :on-click #(delete! post)}
     [icons/square]]))

(defn bad-link-in-map-indexed [posts]
  (map-indexed
   (fn [i post]
     [:a {:aria-label "Read more"
          :href (:url post)}
      (:title post)])
   posts))

(defn bad-role-button-in-map [posts]
  (map (fn [post]
         [:div {:aria-label "Select post"
                :on-click #(select! post)
                :role "button"}])
       posts))

(defn bad-img-in-for [posts]
  (for [post posts]
    [:img {:alt "Post thumbnail"
           :src (:thumbnail post)}]))

(defn bad-aliased-button-in-for [posts]
  (for [post posts]
    [ui/button {:aria-label "Pin post"
                :on-click #(pin! post)}]))

(defn bad-built-attrs-in-for [posts base]
  (for [post posts]
    [:button (assoc base :aria-label "Share post")]))

(defn bad-nested-in-keep [sections]
  (keep (fn [section]
          (when (seq (:posts section))
            [:div
             [:a {:aria-label "Open section"
                  :href (:url section)}]]))
        sections))

(defn ok-computed-name [posts]
  (for [post posts]
    [:button {:aria-label (str "Remove " (:title post))
              :on-click #(delete! post)}
     [icons/square]]))

(defn ok-decorative-alt [posts]
  (for [post posts]
    [:img {:alt ""
           :src (:thumbnail post)}]))

(defn ok-img-inside-named-control [posts]
  (for [post posts]
    [:button {:aria-label (str "Open " (:title post))}
     [:img {:alt "Post thumbnail"
            :src (:thumbnail post)}]]))

(defn ok-visible-text-only [posts]
  (for [post posts]
    [:button {:on-click #(delete! post)}
     "Remove"]))

(defn ok-name-in-binding [posts]
  (for [post posts
        :let [fallback [:button {:aria-label "Retry"}]]]
    [:div (or (:title post) fallback)]))

(defn ok-status-icon [posts]
  (for [post posts]
    [:svg {:aria-label "Published"
           :role "img"}]))

(defn ok-outside-any-loop [post]
  [:button {:aria-label "Remove post"
            :on-click #(delete! post)}
   [icons/square]])

(defn ok-dynamic-attrs [posts build-props]
  (for [post posts]
    [:button (build-props post)
     "Remove"]))
