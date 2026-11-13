(ns blogapp.data-vectors
  (:require [blogapp.other :as other]
            [blogapp.ui :as ui :refer [icon-x]]))

(defn- ok-chip-shell [{:keys [href on-click]}]
  (if href
    [:a {:href href
         :on-click on-click}]
    [:button {:on-click on-click
              :type "button"}]))

(defn chip [props label]
  (let [[tag attrs] (ok-chip-shell props)]
    [tag attrs label]))

(defn- ok-into-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn into-button [on-click label]
  (into (ok-into-shell on-click) [label]))

(defn- ok-conj-shell [href]
  [:a {:href href}])

(defn conj-link [href label]
  (conj (ok-conj-shell href) label))

(defn- ok-into-unread-children-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn into-children [on-click children]
  (into (ok-into-unread-children-shell on-click) children))

(defn- ok-shell-rendered-only-in-dead-code [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn dead-code-callers [on-click]
  #_[:div (ok-shell-rendered-only-in-dead-code on-click)]
  (into (ok-shell-rendered-only-in-dead-code on-click) ["Save"]))

(comment
  [:div (ok-shell-rendered-only-in-dead-code identity)])

(defn- ok-shell-rebuilt-inside-wrapper [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn wrapped-fill [on-click label]
  (let [[tag attrs] (ok-shell-rebuilt-inside-wrapper on-click)]
    [:div {:class "row"}
     [tag attrs label]]))

(defn- ok-shell-with-local-namesake [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn namesake-callers [on-click label]
  [:div
   (into (ok-shell-with-local-namesake on-click) [label])
   (let [ok-shell-with-local-namesake (fn [] [:span label])]
     (ok-shell-with-local-namesake))])

(defn- bad-bound-whole-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn bound-whole [on-click]
  (let [shell (bad-bound-whole-shell on-click)]
    shell))

(defn- bad-conj-child-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn toolbar [on-click]
  (conj [:div] (bad-conj-child-shell on-click)))

(defn- bad-shell-after-sibling-child [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn row [on-click]
  [:div
   [:span "Name"]
   (bad-shell-after-sibling-child on-click)])

(defn- bad-shell-also-rendered [props]
  [:button {:on-click (:on-click props)
            :type "button"}])

(defn filled-and-rendered [props label]
  [:div
   (let [[tag attrs] (bad-shell-also-rendered props)]
     [tag attrs label])
   (bad-shell-also-rendered props)])

(defn- bad-shell-filled-only-in-dead-code [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn rendered-with-dead-fill [on-click]
  [:div (bad-shell-filled-only-in-dead-code on-click)])

(comment
  (into (bad-shell-filled-only-in-dead-code identity) ["Save"]))

(defn- bad-shell-filled-only-in-other-namespace [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn other-namespace-fill [props label]
  (let [[tag attrs] (other/bad-shell-filled-only-in-other-namespace props)]
    [tag attrs label]))

(defn- bad-shell-shadowed-by-local [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn local-fill [label]
  (let [bad-shell-shadowed-by-local (fn [] [:span])]
    (into (bad-shell-shadowed-by-local) [label])))

(defn- bad-shell-passed-as-value [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn shells [handlers]
  (map bad-shell-passed-as-value handlers))

(defn- bad-shell-filled-with-icon-by-into [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn close-button [on-click]
  (into (bad-shell-filled-with-icon-by-into on-click) [[icon-x {}]]))

(defn- bad-shell-filled-with-icon-by-conj [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn close-button-conj [on-click]
  (conj (bad-shell-filled-with-icon-by-conj on-click) [icon-x {}]))

(defn- bad-shell-rebuilt-without-child [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn rebuilt [on-click]
  (let [[tag attrs] (bad-shell-rebuilt-without-child on-click)]
    [tag attrs]))

(defn- bad-multi-arity-shell
  ([on-click]
   [:button {:on-click on-click
             :type "button"}])
  ([on-click _]
   (bad-multi-arity-shell on-click)))

(defn multi-arity-fill [on-click label]
  (into (bad-multi-arity-shell on-click) [label]))

(defn bad-button-holding-a-local-component [options option-component]
  [:div {:aria-label "Options"
         :role "listbox"}
   (for [option options]
     ^{:key option}
     [:button {:on-click #(prn option)
               :type "button"}
      [option-component {:option option}]])])

(defn bad-button-holding-an-icon-prop [{:keys [icon on-click]}]
  [:button {:on-click on-click
            :type "button"}
   [icon]])

(defn bad-button-holding-an-icon-component [on-click]
  [:button {:on-click on-click
            :type "button"}
   [ui/icon-x {}]])

(defn bad-button-holding-a-referred-icon [on-click]
  [:button {:on-click on-click
            :type "button"}
   [icon-x {}]])

(defn- bad-unreturned-button [log on-click]
  (swap! log conj [:button {:on-click on-click
                            :type "button"}])
  [:a {:href "/"}])

(defn unreturned-link [log on-click label]
  (into (bad-unreturned-button log on-click) [label]))

(defn- bad-shell-in-for-binding [props]
  [:button {:on-click (:on-click props)
            :type "button"}])

(defn for-fill [props label]
  (for [[tag attrs] (bad-shell-in-for-binding props)]
    [tag attrs label]))

(defn- bad-shell-in-let-body [props]
  [:button {:on-click (:on-click props)
            :type "button"}])

(defn let-body [props gap label]
  (let [spacing gap]
    [[:span {:class spacing} label] (bad-shell-in-let-body props)]))

(defn- bad-shell-rebuilt-inside-other-element [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn wrapped [on-click]
  (let [[tag attrs] (bad-shell-rebuilt-inside-other-element on-click)]
    [:div {:class "wrapper"} [tag attrs]]))

(defn- bad-shell-rebuilt-only-in-bindings [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn rebuilt-in-bindings [on-click label]
  (let [[tag attrs] (bad-shell-rebuilt-only-in-bindings on-click)
        labelled [tag attrs label]]
    (prn labelled)
    [tag attrs]))

(defn- bad-shell-applied [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn applied [args label]
  (let [[tag attrs] (apply bad-shell-applied args)]
    [tag attrs label]))

(defn bad-public-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn public-fill [on-click]
  (into (bad-public-shell on-click) ["Save"]))

(defn ^:private ok-meta-private-shell [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn meta-private-fill [on-click]
  (into (ok-meta-private-shell on-click) ["Save"]))

(defn- bad-shell-rebuilt-with-icon [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn icon-rebuild [on-click]
  (let [[tag attrs] (bad-shell-rebuilt-with-icon on-click)]
    [tag attrs [icon-x {}]]))

(defn- bad-shell-rebuilt-empty-in-one-branch [on-click]
  [:button {:on-click on-click
            :type "button"}])

(defn branch-rebuild [on-click label]
  (let [[tag attrs] (bad-shell-rebuilt-empty-in-one-branch on-click)]
    (if label
      [tag attrs label]
      [tag attrs])))
