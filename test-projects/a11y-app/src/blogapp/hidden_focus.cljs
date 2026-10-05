(ns blogapp.hidden-focus)

(defn ok-decorative-icon []
  ;; nothing focusable: hiding it is how a decorative icon is written
  [:svg {:aria-hidden true
         :class "icon"}])

(defn ok-hidden-wrapper []
  [:div {:aria-hidden true}
   "Duplicated for layout"])

(defn ok-mouse-only-affordance [on-scrub]
  ;; out of the tab order, and mouse focus blocked, so hiding it is safe
  [:button {:aria-hidden true
            :tab-index -1
            :on-mouse-down #(.preventDefault ^js %)
            :on-click on-scrub}
   "Scrub"])

(defn ok-camel-case-escape-hatch [on-scrub]
  [:button {:aria-hidden true
            :tabIndex -1
            :on-click on-scrub}
   "Scrub"])

(defn ok-aria-hidden-false []
  [:button {:aria-hidden false} "Publish"])

(defn ok-computed-aria-hidden [hidden?]
  ;; conservative: non-literal value — skipped
  [:button {:aria-hidden hidden?} "Publish"])

(defn ok-built-attrs [props]
  ;; conservative: the tabindex may live in the part that cannot be read
  [:button (assoc props :aria-hidden true) "Publish"])

(defn ok-link-without-href []
  ;; an <a> with no href is not focusable
  [:a {:aria-hidden true} "Draft"])

(defn bad-hidden-button [on-scrub]
  [:button {:aria-hidden true
            :on-click on-scrub}
   "Scrub"])

(defn bad-hidden-link [url]
  [:a {:aria-hidden true
       :href url}
   "Read more"])

(defn bad-hidden-input []
  [:input {:aria-hidden true
           :type "text"}])

(defn bad-hidden-select []
  [:select {:aria-hidden true}
   [:option "Newest"]])

(defn bad-hidden-role-widget [on-toggle]
  [:div {:aria-hidden true
         :role "switch"
         :aria-checked false
         :on-click on-toggle}
   "Night mode"])

(defn bad-hidden-tabbable-div []
  [:div {:aria-hidden true
         :tab-index 0}
   "Skip to comments"])

(defn bad-hidden-string-true []
  [:button {:aria-hidden "true"} "Publish"])

(defn bad-hidden-keyword-true []
  [:button {:aria-hidden :true} "Publish"])

(defn ok-roving-tabindex [items active?]
  ;; the roving-tabindex idiom: the tabindex cannot be read, so neither can the way out
  (for [item items]
    [:li {:role "treeitem"
          :aria-hidden true
          :tab-index (if (active? item) 0 -1)}
     (:label item)]))

(defn ok-symbol-tabindex [tabindex]
  [:button {:aria-hidden true
            :tab-index tabindex}
   "Scrub"])

(defn ok-disabled-control []
  ;; a disabled control is out of the tab order already
  [:button {:aria-hidden true
            :disabled true}
   "Publish"])

(defn ok-nil-href []
  ;; Reagent omits a nil attribute, so this renders <a> with no href
  [:a {:aria-hidden true
       :href nil}
   "Draft"])

(defn bad-complete-built-attrs []
  ;; every part of the built map is readable, so it states its whole key set
  [:button (assoc {:class "scrub"} :aria-hidden true) "Publish"])

(defn bad-disabled-non-form-tag [on-toggle]
  ;; `disabled` is inert on a :div, so the tab stop survives it
  [:div {:role "button"
         :tab-index 0
         :aria-hidden true
         :disabled true
         :on-click on-toggle}
   "Toggle"])

(defn bad-disabled-anchor [url]
  [:a {:href url
       :aria-hidden true
       :disabled true}
   "Read"])

(defn bad-tabindex-false []
  ;; React drops a boolean from a numeric attribute, so this is an ordinary tab stop
  [:button {:aria-hidden true
            :tab-index false}
   "Publish"])

(defn ok-hidden-wrapper-child-removed [on-scrub]
  [:div {:aria-hidden true}
   [:button {:on-click on-scrub
             :tab-index -1}
    "Scrub"]])

(defn ok-hidden-wrapper-opaque-child [props]
  ;; the child's props cannot be read, so its way out cannot be called missing
  [:div {:aria-hidden true}
   [:button props "Scrub"]])

(defn bad-hidden-wrapper [on-scrub]
  [:div {:aria-hidden true}
   [:button {:on-click on-scrub} "Duplicate for layout"]])

(defn bad-hidden-wrapper-deep [url]
  [:div {:aria-hidden true}
   [:div {:class "row"}
    [:span "text"]
    [:a {:href url} "Link"]]])

(defn ok-hidden-wrapper-hidden-input []
  ;; an input of type hidden renders nothing and can never take focus
  [:div {:aria-hidden true}
   [:input {:type "hidden"
            :name "post-id"}]])

(defn ok-hidden-wrapper-descendant-prop []
  ;; the :tooltip button is the span's to place, not part of this subtree
  [:div {:aria-hidden true}
   [:span {:tooltip [:button "Help"]} "text"]])

(defn ok-hidden-wrapper-disabled-fieldset [on-save]
  ;; a disabled fieldset takes every control under it out of the tab order
  [:div {:aria-hidden true}
   [:fieldset {:disabled true}
    [:button {:on-click on-save} "Save"]]])

(defn ok-outer-of-stacked-hidden-wrappers [on-save]
  ;; the inner wrapper answers for this subtree, so the outer does not report twice
  [:div {:aria-hidden true}
   [:section {:aria-hidden true}
    [:button {:on-click on-save} "Save"]]])
