(ns webapp.test-id-views
  (:require
   [clojure.string :as str]
   [webapp.components :refer [design-button icon icon-for join-id keyword-fn]]))

(def const-id "const-button")

(defn views [attrs label kind role on-pick]
  [:div
   [:button {:aria-label "Send"
             :data-testid "send-button"}
    [icon]]
   [:button {:data-testid "save-button"} "Save"]
   [:a {:data-testid "home-link"
        :href "/home"}
    [:span "Home"]]
   [:textarea {:aria-label "Edit section"
               :data-testid "edit-area"}]
   [:input {:aria-label "Agree"
            :data-testid "agree-box"
            :type "checkbox"}]
   [:input {:aria-label "Query"
            :data-testid "query-input"}]
   [:input {:aria-label "Find"
            :data-testid "find-input"
            :type :search}]
   [:input {:aria-label "Pick"
            :data-testid "pick-input"
            :list "options"}]
   [:input {:data-testid "go-input"
            :type "submit"
            :value "Go"}]
   [:select {:aria-label "Country"
             :data-testid "country-select"}]
   [:select {:aria-label "Tags"
             :data-testid "tags-select"
             :multiple true}]
   [:div {:aria-label "Settings"
          :data-testid "settings-dialog"
          :role "dialog"}]
   [:div {:data-testid "content-scroll"} "Content"]
   [:button {:data-testid "status-badge"} "Status"]
   [:div {:data-testid "status-badge"}]
   [design-button {:data-testid "component-button"} "Go"]
   [:button {:data-testid "hidden-button"
             :hidden true}
    "Hide"]
   [:div {:aria-hidden true}
    [:button {:data-testid "inner-button"} "Inner"]]
   [:button {:data-testid "icon-only"} [icon]]
   [:button {:data-testid "symbol-label"} label]
   [:button {:data-testid "hidden-text"} [:span {:aria-hidden true} "Hidden"]]
   [:input {:data-testid "bare-input"}]
   [:input {:aria-label "File"
            :data-testid "file-input"
            :type "file"}]
   [:input {:aria-label "Kind"
            :data-testid "kind-input"
            :type kind}]
   [:a {:data-testid "no-href-link"} "Nowhere"]
   [:div {:aria-label "Decor"
          :data-testid "decor-div"
          :role "presentation"}]
   [:div {:aria-label "Mystery"
          :data-testid "mystery-div"
          :role role}]
   [:button {:data-testid "const-button"} "Constant"]
   [:button (merge {:data-testid "merged-button"} attrs) "Merged"]
   [:select {:aria-label "Sized"
             :data-testid "sized-select"
             :size 4}]
   [:button {:data-testid "picked-button"
             :on-click on-pick}
    "Picked"]
   [:input {:aria-label "Listed"
            :data-testid "listed-box"
            :list "choices"
            :type "checkbox"}]
   [:a {:data-testid "blank-text" :href "/blank"} " "]
   [:div {:data-testid "text-dialog"
          :role "dialog"}
    "Hello"]
   [:button {:aria-label "label-as-id"} "Labelled"]
   [:button {:data-testid "dual-role"} "Dual"]
   [:a {:data-testid "dual-role"
        :href "/dual"}
    "Dual"]
   [:button {:aria-label "Third"} {:data-testid "third-map"}]
   [:my-element {:aria-label "Custom"
                 :data-testid "custom-element"
                 :role "button"}]
   [:button {:data-testid "empty-id"} ""]
   [:button {:data-testid "labelled-button"} "Labelled button"]
   [:button {:title :data-testid "odd-key" "Odd"} "Odd"]])

(defn reviewed [kind x multi? open? url]
  [:div
   [:button {:data-testid "svg-button"} [:svg {:view-box "0 0 24 24"} [:path {:d "M0 0L10 10"}]]]
   [:button {:data-testid "class-button"} [:i {:class "fa fa-close"}]]
   [:button {:data-testid "img-button"} [:img {:alt "" :src "/close.svg"}]]
   [:button {:data-testid "comp-text"} [icon {:name "close"}]]
   [:button {:data-testid "fn-text"} (icon-for "close")]
   [:button {:data-testid "case-button"} (case kind "a" [:span] nil)]
   [:button {:data-testid "when-text"} (when x "Maybe")]
   [:select {:aria-label "One" :data-testid "size-one" :size 1}]
   [:select {:aria-label "Multi" :data-testid "multi-var" :multiple multi?}]
   [:a {:data-testid "href-var" :href (when open? url)} "Link"]
   [:a {:data-testid "empty-href" :href ""} "Home"]
   [:input {:aria-label "Empty" :data-testid "empty-list" :list ""}]
   [:input {:aria-label "Var" :data-testid "list-var" :list url}]
   [:button {:aria-hidden (not open?) :data-testid "aria-hidden-var"} "AH"]
   [:div {:aria-hidden open?} [:button {:data-testid "aria-hidden-anc"} "AHA"]]
   [:button {:data-testid "style-hidden" :style {:display "none"}} "S"]
   [:div {:style {:visibility "hidden"}} [:button {:data-testid "vis-parent"} "V"]]
   [:button {:data-testid "style-child"} [:span {:style {:display "none"}} "Gone"]]
   [:button {:data-testid "style-var" :style x} "SV"]
   [:button {:aria-label "E" :data-testid "exact"}]
   [:div {:data-testid "exact-other"}]
   [:button {:aria-label "K" :data-testid "kw-collide"}]
   [:div {:data-testid :kw-collide}]
   [:button {:aria-label "S" :data-testid "str-collide"}]
   [:div {:data-testid (str "str-" "coll" "ide")}]
   [:button {:aria-label "W" :data-testid "ws-collide"}]
   [:div {:data-testid " ws-collide "}]
   [:button {:aria-labelledby "missing" :data-testid "labelledby-only"}]
   [:button {:aria-label " " :data-testid "blank-label"}]
   [:input {:data-testid "image-value" :type "image" :value "Go"}]
   [:input {:data-testid "text-role-button" :role "button" :value "V"}]
   [:input {:aria-label "H" :data-testid "hidden-role" :role "button" :type "hidden"}]
   [:span {:aria-label "Kept"}
    [:button {:data-testid "plain-in-span"} [:span "Plain"]]]
   [:div (merge {:class "c"} x)
    [:button {:data-testid "in-dynamic-div"} "Dyn"]]
   [:button {:aria-label "Spaced" :data-testid "inner  space"}]
   [:button {:data-testid "hidden-false" :hidden false} "HF"]
   [:a {:data-testid "nil-href" :href nil} "N"]
   [:input {:aria-label "LN" :data-testid "list-nil" :list nil}]
   [:button {:data-testid "string-style" :style {"display" "none"}} "SS"]
   [design-button {:kind :panel}
    [:button {:data-testid "in-component"} "IC"]]
   [:button {:aria-label x :data-testid "label-sym"}]
   [:button {:data-testid "titled" :title "T"}]
   [:button {:data-testid "button-value" :type "button" :value "Valued"}]
   [:input {:data-testid "no-value-button" :type "button"}]
   [:span (str "built-key" "-free")]
   [:button {:aria-label "BK" :data-testid "built-key-free"}]
   [:div {:data-testid (join-id "call-" "built-zz")}]
   [:button {:aria-label "CB" :data-testid "call-built-zz"}]
   [:div {:data-testid (str "pre-" x)}]
   [:button {:aria-label "P" :data-testid "pre-one"}]
   [:div {:data-testid (str x "-tail")}]
   [:button {:aria-label "T" :data-testid "x-tail"}]
   [:button {:aria-label "O" :data-testid "other-one"}]
   [:button {:aria-label "KE" :data-testid "kw-elsewhere"}]
   (keyword-fn :kw-elsewhere)])

(defn second-review [props open? n x kind]
  [:div
   [:button {:data-testid "save-top"} "Save twice"]
   [:button {:data-testid "save-bottom"} "Save twice"]
   [:button {:on-click x} "Delete everything"]
   [:button {:data-testid "delete-all"} "Delete everything"]
   [:div props [:button {:data-testid "under-param-attrs"} "Under param"]]
   (let [attrs (when-not open? {:hidden true})]
     [:div attrs [:button {:data-testid "under-let-attrs"} "Under let"]])
   [:dialog [:button {:data-testid "in-closed-dialog"} "In closed"]]
   [:dialog {:open true} [:button {:data-testid "in-open-dialog"} "In open"]]
   [:template [:button {:data-testid "in-template"} "In template"]]
   [:button {:data-testid "role-empty" :role ""} "Role empty"]
   [:select {:aria-label "Zero" :data-testid "size-zero" :size 0}]
   [:select {:aria-label "Multi zero" :data-testid "multiple-zero" :multiple 0}]
   [:a {:data-testid "href-true" :href true} "Href true"]
   [:button {:data-testid "padded-none" :style {:display " none "}} "Padded"]
   [:button {:data-testid "upper-none" :style {:display "NONE"}} "Upper"]
   [:div {:data-testid "menu-titled" :role "menu" :title "Actions"}]
   (let [id (str "item-" n)]
     [:div {:data-testid id}])
   [:button {:aria-label "Item one" :data-testid "item-1"}]
   [:div {:data-testid 'sym-id}]
   [:button {:aria-label "Sym" :data-testid "sym-id"}]
   [:div {:data-testid (->> "zz9" (str "qq7"))}]
   [:button {:aria-label "Threaded" :data-testid "qq7zz9"}]
   [:div {:data-testid (str/join "~" ["yy5" "ww3"])}]
   [:button {:aria-label "Joined" :data-testid "yy5~ww3"}]
   [:div {:data-testid (str "mid-" x "-dle-" kind "-end")}]
   [:button {:aria-label "Mid" :data-testid "mid-a-dle-b-end"}]
   [:button {:aria-label "Mid other" :data-testid "mid-a-end"}]
   #_[:button {:data-testid "only-discarded"} "Discarded"]
   (comment [:button {:data-testid "only-in-comment"} "Commented"])
   [:button {:data-testid "icon-and-text"} [icon] "Icon text"]])

(defn mutation-cases [d v]
  [:div
   [:button {:data-testid "hidden-nil" :hidden nil} "Hidden nil"]
   [:button {:data-testid "with-break"} "Line" [:br] "Two"]
   [:button {:data-testid "display-var" :style {:display d}} "Display var"]
   [:button {:data-testid "visibility-var" :style {:visibility v}} "Visibility var"]
   [:button {:data-testid "uneval-child"} "Kept" #_[icon]]
   (let [id "bound-id"]
     [:div {:data-testid id}])
   [:button {:aria-label "Bound" :data-testid "bound-id"}]
   [:div {:data-testid (str "single-zz")}]
   [:button {:aria-label "Single more" :data-testid "single-zz-more"}]])

(defn pass-through [{:keys [data-testid]} label]
  [:div {:data-testid data-testid} label])
