(ns blogapp.nesting)

(defn ok-siblings-in-wrapper [on-open on-remove]
  ;; the wrapper positions, the controls are siblings
  [:div {:class "post-row"}
   [:button {:on-click on-open} "Open"]
   [:button {:on-click on-remove} "Remove"]])

(defn ok-button-with-icon-markup [on-open]
  [:button {:on-click on-open}
   [:span {:class "icon"}]
   "Open"])

(defn ok-anchor-without-href [on-open]
  ;; an <a> with no href is not interactive, so nothing is nested inside a control
  [:a {:on-click on-open}
   [:button {:on-click on-open} "Open"]])

(defn ok-presentational-role-wrapper [on-open]
  [:div {:role "presentation"}
   [:button {:on-click on-open} "Open"]])

(defn ok-computed-wrapper-attrs [props on-open]
  ;; conservative: the wrapper's role cannot be read
  [:div props
   [:button {:on-click on-open} "Open"]])

(defn bad-button-in-button [on-scrub on-remove]
  [:button {:on-click on-scrub}
   "Scrub"
   [:button {:on-click on-remove} "Remove"]])

(defn bad-button-under-layout-divs [on-scrub markers on-pick]
  [:button {:on-click on-scrub}
   [:div {:class "track"}
    [:div {:class "markers"}
     (for [marker markers]
       ^{:key (:id marker)} [:button {:on-click #(on-pick marker)} (:label marker)])]]])

(defn bad-link-in-link [url]
  [:a {:href url}
   "Read the post"
   [:a {:href (str url "#comments")} "Comments"]])

(defn bad-button-in-link [url on-remove]
  [:a {:href url}
   "Read the post"
   [:button {:on-click on-remove} "Remove"]])

(defn bad-role-button-wrapper [on-scrub markers on-pick]
  [:div {:role "button"
         :aria-label "Scrub the strip"
         :on-click on-scrub}
   (for [marker markers]
     ^{:key (:id marker)} [:button {:on-click #(on-pick marker)} (:label marker)])])

(defn bad-role-link-inside-button [on-open]
  [:button {:on-click on-open}
   "Open"
   [:span {:role "link"
           :aria-label "Permalink"
           :tab-index 0}]])

(defn ok-hiccup-passed-as-prop [on-save]
  ;; the :tooltip vector is markup handed to the button, not markup nested in it
  [:button {:on-click on-save
            :tooltip [:a {:href "/help"} "Help"]}
   "Save"])

(defn ok-quoted-child [on-open render]
  ;; quoted hiccup is data, not something that renders
  [:button {:on-click on-open}
   (render '[:div [:a {:href "/x"} "quoted"]])])

(defn ok-nil-href-container [on-remove]
  [:a {:href nil}
   "Draft"
   [:button {:on-click on-remove} "Remove"]])

(defn ok-props-built-by-a-call [build on-save]
  ;; the markup goes to the props the call builds, not to the button's body
  [:button (build {:tooltip [:a {:href "/help"} "Help"]}) "Save"])

(defn ok-discarded-child [on-save]
  [:button {:on-click on-save}
   #_[:a {:href "/x"} "gone"]
   "Save"])

(defn ok-hidden-input [on-save]
  ;; an input of type hidden renders nothing and is not interactive content
  [:button {:on-click on-save}
   "Save"
   [:input {:type "hidden"
            :name "post-id"}]])

(defn bad-input-in-button [on-save]
  [:button {:on-click on-save}
   "Save"
   [:input {:type "text"}]])

(defn bad-select-in-link [url]
  [:a {:href url}
   "Sort"
   [:select [:option "Newest"]]])

(defn bad-label-in-button [on-save]
  [:button {:on-click on-save}
   [:label "Name"]])
