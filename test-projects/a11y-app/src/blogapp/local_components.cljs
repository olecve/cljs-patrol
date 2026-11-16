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

(defn ok-button-holding-a-component-returning-a-call [on-click]
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

(defn- icon-slot [icon]
  [:span.icon-slot icon])

(defn bad-button-holding-an-icon-through-a-parameter [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-slot [icons/square {:size 16}]]])

(defn ok-button-holding-text-through-a-parameter [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-slot "Close"]])

(defn- icon-face [{:keys [icon]}]
  icon)

(defn bad-button-holding-an-icon-through-a-destructured-key [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-face {:icon [icons/square {:size 16}]}]])

(defn ok-button-holding-text-through-a-destructured-key [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-face {:icon "Close"}]])

(defn ok-button-holding-a-symbol-through-a-destructured-key [on-click label]
  [:button {:on-click on-click
            :type "button"}
   [icon-face {:icon label}]])

(defn ok-button-holding-a-key-the-map-leaves-out [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-face {:size 16}]])

(defn- renamed-face [{text :label}]
  [:span text])

(defn bad-button-holding-an-icon-through-a-renamed-key [on-click]
  [:button {:on-click on-click
            :type "button"}
   [renamed-face {:label [icons/square {:size 16}]}]])

(defn ok-button-holding-text-through-a-renamed-key [on-click]
  [:button {:on-click on-click
            :type "button"}
   [renamed-face {:label "Close"}]])

(defn- whole-face [{:as props}]
  [:span props])

(defn ok-button-holding-a-whole-map-parameter [on-click]
  [:button {:on-click on-click
            :type "button"}
   [whole-face {:label [icons/square {:size 16}]}]])

(defn- shadowing-slot [icon]
  (let [icon "Close"]
    [:span icon]))

(defn ok-button-holding-a-parameter-rebound-inside [on-click]
  [:button {:on-click on-click
            :type "button"}
   [shadowing-slot [icons/square {:size 16}]]])

(defn- arity-slot
  ([] [:span "Close"])
  ([icon] [:span icon]))

(defn bad-button-holding-an-icon-through-the-matching-arity [on-click]
  [:button {:on-click on-click
            :type "button"}
   [arity-slot [icons/square {:size 16}]]])

(defn- variadic-slot [icon & _more]
  [:span icon])

(defn bad-button-holding-an-icon-through-a-variadic-component [on-click]
  [:button {:on-click on-click
            :type "button"}
   [variadic-slot [icons/square {:size 16}]]])

(defn- rest-slot [icon & children]
  [:span icon children])

(defn ok-button-holding-a-rest-parameter [on-click]
  [:button {:on-click on-click
            :type "button"}
   [rest-slot [icons/square {:size 16}] [icons/square {:size 16}] [icons/square {:size 16}]]])

(def save-caption "Save")

(defn- caption-label []
  save-caption)

(defn ok-button-holding-a-component-returning-a-text-var [on-click]
  [:button {:on-click on-click
            :type "button"}
   [caption-label]])

(defn- optional-icon-slot [icon]
  [:span (when icon icon)])

(defn bad-button-holding-an-icon-through-a-branch [on-click]
  [:button {:on-click on-click
            :type "button"}
   [optional-icon-slot [icons/square {:size 16}]]])

(defn ok-button-holding-text-through-a-branch [on-click]
  [:button {:on-click on-click
            :type "button"}
   [optional-icon-slot "Close"]])

(defn bad-button-holding-an-icon-through-a-let-bound-map [on-click]
  (let [props {:icon [icons/square {:size 16}]}]
    [:button {:on-click on-click
              :type "button"}
     [icon-face props]]))

(defn ok-button-holding-text-through-a-let-bound-map [on-click]
  (let [props {:icon "Close"}]
    [:button {:on-click on-click
              :type "button"}
     [icon-face props]]))

(def icon-props {:icon [icons/square {:size 16}]})

(defn bad-button-holding-an-icon-through-a-def-map [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-face icon-props]])

(def close-text "Close")

(def close-caption (when close-text close-text))

(defn- caption-ignoring-its-parameter [close-text]
  (prn close-text)
  close-caption)

(defn ok-button-holding-a-var-that-shares-a-parameter-name [on-click]
  [:button {:on-click on-click
            :type "button"}
   [caption-ignoring-its-parameter [icons/square {:size 16}]]])

(defn- translated-face [{:keys [k]}]
  (translate k))

(defn ok-button-holding-a-component-returning-a-translation [on-click]
  [:button {:on-click on-click
            :type "button"}
   [translated-face {:k :save}]])

(defn- built-label []
  (into [:span] ["Save"]))

(defn ok-button-holding-a-component-returning-a-built-vector [on-click]
  [:button {:on-click on-click
            :type "button"}
   [built-label]])

(defn- keyed-label []
  ^{:key "save"} [:span "Save"])

(defn ok-button-holding-a-component-returning-a-keyed-vector [on-click]
  [:button {:on-click on-click
            :type "button"}
   [keyed-label]])

(defn- hidden-wrap [content]
  [:span {:aria-hidden true} content])

(defn bad-button-holding-text-a-component-hides [on-click]
  [:button {:on-click on-click
            :type "button"}
   [hidden-wrap "Save"]])

(defn bad-button-holding-a-symbol-a-component-hides [on-click label]
  [:button {:on-click on-click
            :type "button"}
   [hidden-wrap label]])

(defn- drop-face [_icon _label]
  [:svg])

(defn bad-button-holding-text-a-component-drops [on-click]
  [:button {:on-click on-click
            :type "button"}
   [drop-face [:svg] "Close"]])

(defn bad-button-holding-a-map-a-component-drops [on-click]
  [:button {:on-click on-click
            :type "button"}
   [drop-face {:size 16} {:label "Close"}]])

(def caption-text "Close")

(def caption-props {:icon caption-text})

(defn- caption-face [caption-text]
  (prn caption-text)
  [icon-face caption-props])

(defn ok-button-holding-a-def-map-naming-a-var [on-click]
  [:button {:on-click on-click
            :type "button"}
   [caption-face [icons/square {:size 16}]]])

(defn- either-slot
  ([icon] [:span icon])
  ([_icon & _more] [:span "Close"]))

(defn bad-button-holding-an-icon-through-a-fixed-arity [on-click]
  [:button {:on-click on-click
            :type "button"}
   [either-slot [icons/square {:size 16}]]])

(defn- avatar [user]
  [:img {:alt (:name user)
         :src (:url user)}])

(defn ok-button-holding-a-component-with-a-dynamic-alt [user on-click]
  [:button {:on-click on-click
            :type "button"}
   [avatar user]])

(defn- named-avatar [user-name]
  [:img {:alt user-name
         :src "avatar.png"}])

(defn ok-button-holding-a-component-with-an-alt-argument [on-click]
  [:button {:on-click on-click
            :type "button"}
   [named-avatar "Ann"]])

(defn bad-button-holding-a-component-with-an-empty-alt-argument [on-click]
  [:button {:on-click on-click
            :type "button"}
   [named-avatar ""]])

(declare hub-b
         hub-c
         hub-d
         hub-e
         hub-f)

(defn- hub-a []
  [:span [icons/square {:size 16}] [hub-b] [hub-c] [hub-d] [hub-e] [hub-f]])

(defn- hub-b []
  [:span [icons/square {:size 16}] [hub-a] [hub-c] [hub-d] [hub-e] [hub-f]])

(defn- hub-c []
  [:span [icons/square {:size 16}] [hub-a] [hub-b] [hub-d] [hub-e] [hub-f]])

(defn- hub-d []
  [:span [icons/square {:size 16}] [hub-a] [hub-b] [hub-c] [hub-e] [hub-f]])

(defn- hub-e []
  [:span [icons/square {:size 16}] [hub-a] [hub-b] [hub-c] [hub-d] [hub-f]])

(defn- hub-f []
  [:span [icons/square {:size 16}] [hub-a] [hub-b] [hub-c] [hub-d] [hub-e]])

(defn ok-button-holding-components-past-the-read-limit [on-click]
  [:button {:on-click on-click
            :type "button"}
   [hub-a]])
