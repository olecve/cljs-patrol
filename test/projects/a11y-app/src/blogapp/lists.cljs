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

(defn bad-img-naming-a-link [posts]
  ;; the link has no name of its own, so the alt text becomes its name
  (for [post posts]
    [:a {:href (:url post)}
     [:img {:alt "Post thumbnail"
            :src (:thumbnail post)}]]))

(defn ok-img-badge-in-row [posts]
  ;; the image names nothing: it sits in the row, so repeating its alt is correct
  (for [post posts]
    [:div
     [:img {:alt "Verified"
            :src "/verified.svg"}]
     (:title post)]))

(defn bad-aliased-button-in-for [posts]
  (for [post posts]
    [ui/button {:aria-label "Pin post"
                :on-click #(pin! post)}]))

(defn ok-built-attrs-in-for [posts base]
  ;; a built map is a partial view: `base` may still supply a per-item name
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

(defn bad-name-in-for-let [posts]
  ;; :let is re-evaluated on every iteration, so this button does repeat
  (for [post posts
        :let [fallback [:button {:aria-label "Retry"}]]]
    [:div (or (:title post) fallback)]))

(defn ok-name-in-first-binding-collection []
  ;; the first binding's collection expression is evaluated exactly once
  (for [item (cons [:button {:aria-label "Add"}] (list))]
    [:div item]))

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

(defn ok-branch-arms-named-differently [tabs]
  ;; only one arm renders per item, and they do not share a name
  (for [tab tabs]
    (case tab
      :home [:a {:aria-label "Home"
                 :href "/"} "h"]
      :settings [:a {:aria-label "Settings"
                     :href "/s"} "s"])))

(defn bad-branch-with-one-name [posts]
  ;; the other arm renders nothing, so every item this does render says the same
  (for [post posts]
    (if (:deleted? post)
      nil
      [:button {:aria-label "Remove post"} "x"])))

(defn ok-map-collection-argument [f posts]
  ;; the collection argument is evaluated once; only the function repeats
  (map f (conj posts [:button {:aria-label "Add post"} "+"])))

(defn ok-decorative-alt-in-a-link [posts]
  ;; the link supplies no name, so only the empty alt keeps this from flagging
  (for [post posts]
    [:a {:href (:url post)}
     [:img {:alt ""
            :src (:thumbnail post)}]]))

(defn ok-img-in-a-built-attrs-control [posts base]
  ;; base may already carry :aria-label, so the control is not known to be unnamed
  (for [post posts]
    [:a (assoc base :href (:url post))
     [:img {:alt "Open post"}]]))
