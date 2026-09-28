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
