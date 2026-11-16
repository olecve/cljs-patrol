(ns blogapp.local-components
  (:require
   [blogapp.icons :as icons]
   [blogapp.ui :as ui]
   [reagent.core :as r]
   [rum.core :as rum]))

(defn- thumbnail-face [{:keys [display-name]}]
  [:<>
   [:div [:img {:alt ""
                :src "x.png"}]]
   [ui/row {:as :div} display-name]])

(defn ok-button-holding-a-component-rendering-a-prop [{:keys [filename on-click]}]
  [:button {:on-click on-click
            :type "button"}
   [thumbnail-face {:display-name filename}]])

(defn- save-label []
  [:span.label "Save"])

(defn ok-button-holding-a-component-rendering-text [on-click]
  [:button {:on-click on-click
            :type "button"}
   [save-label]])

(defn- save-text []
  "Save")

(defn ok-button-holding-a-component-returning-a-string [on-click]
  [:button {:on-click on-click
            :type "button"}
   [save-text]])

(defn- prop-label [{:keys [label]}]
  label)

(defn ok-button-holding-a-component-returning-a-destructured-prop [on-click]
  [:button {:on-click on-click
            :type "button"}
   [prop-label {:label "Save"}]])

(defn- parameter-label [label]
  label)

(defn ok-button-holding-a-component-returning-a-parameter [on-click label]
  [:button {:on-click on-click
            :type "button"}
   [parameter-label label]])

(defn- caption [label]
  (if label
    [:span label]
    [icons/square {:size 16}]))

(defn ok-button-holding-a-component-with-one-text-branch [on-click label]
  [:button {:on-click on-click
            :type "button"}
   [caption label]])

(defn- labelled-face []
  [:span [save-label]])

(defn ok-button-holding-a-component-two-deep [on-click]
  [:a {:href "/save"
       :on-click on-click}
   [labelled-face]])

(r/defc reagent-label [{:keys [text]}]
  [:span text])

(defn ok-button-holding-a-reagent-defc [on-click]
  [:button {:on-click on-click
            :type "button"}
   [reagent-label {:text "Close"}]])

(rum/defc rum-label < rum/static []
  [:span "Close"])

(defn ok-button-holding-a-rum-defc [on-click]
  [:button {:on-click on-click
            :type "button"}
   [rum-label]])

(defn- close-icon []
  [:svg {:aria-hidden true}
   [:path {:d "M0 0L16 16"}]])

(defn bad-button-holding-a-component-rendering-an-icon [on-click]
  [:button {:on-click on-click
            :type "button"}
   [close-icon]])

(defn- decorative-image []
  [:div [:img {:alt ""
               :src "x.png"}]])

(defn bad-button-holding-a-component-rendering-a-decorative-image [on-click]
  [:button {:on-click on-click
            :type "button"}
   [decorative-image]])

(defn- render-label [label]
  (str "Label: " label))

(defn bad-button-holding-a-component-returning-a-call [on-click]
  [:button {:on-click on-click
            :type "button"}
   [render-label {:label "Save"}]])

(defn bad-button-holding-an-external-component [on-click]
  [:button {:on-click on-click
            :type "button"}
   [ui/row {:as :div}]])

(defn- self-nesting []
  [:span [self-nesting]])

(defn bad-button-holding-a-self-rendering-component [on-click]
  [:button {:on-click on-click
            :type "button"}
   [self-nesting]])

(declare cycle-back)

(defn- cycle-there []
  [:span [cycle-back]])

(defn- cycle-back []
  [:span [cycle-there]])

(defn bad-button-holding-a-component-cycle [on-click]
  [:button {:on-click on-click
            :type "button"}
   [cycle-there]])

(defn- level-5 [] [:span "Deep"])
(defn- level-4 [] [:span [level-5]])
(defn- level-3 [] [:span [level-4]])
(defn- level-2 [] [:span [level-3]])
(defn- level-1 [] [:span [level-2]])

(defn bad-button-holding-text-past-the-depth-limit [on-click]
  [:button {:on-click on-click
            :type "button"}
   [level-1]])

(defn bad-button-holding-a-shadowed-component [on-click save-label]
  [:button {:on-click on-click
            :type "button"}
   [save-label]])

(def close-markup [:svg])

(defn- close-reference []
  close-markup)

(defn bad-button-holding-a-component-returning-a-var [on-click]
  [:button {:on-click on-click
            :type "button"}
   [close-reference]])
