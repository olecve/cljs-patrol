(ns idsapp.ids
  (:require [blogapp.ui :as ui]))

(defn bad-select-orphan-id []
  [ui/select {:id "orphan-select"}])

(defn bad-textarea-orphan-id []
  [:textarea {:id "orphan-notes"}])

(defn bad-textarea-orphan-shorthand []
  [:textarea#orphan-shorthand])

(defn bad-dialog-orphan-id []
  [:dialog {:id "dialog-orphan"}])

(defn ok-select-label-for []
  [:div
   [:label {:for "paired-select"} "Level"]
   [ui/select {:id "paired-select"}]])

(defn ok-shorthand-label-for []
  [:div
   [:label {:for "paired-shorthand"} "Notes"]
   [:textarea#paired-shorthand]])

(defn ok-id-in-labelledby-list []
  [:div
   [:div {:aria-labelledby "heading listed-id"}]
   [:textarea {:id "listed-id"}]])

(defn ok-cross-file-label []
  [:textarea {:id "cross-file-id"}])

(defn ok-id-from-constant []
  [:textarea {:id "constant-id"}])

(defn ok-computed-id [prefix]
  [:textarea {:id (str prefix "-notes")}])

(defn ok-shorthand-and-id []
  [:textarea#shorthand-side {:id "props-side"}])

(defn ok-id-with-space []
  [:textarea {:id "two words"}])

(defn ok-label-wrapped-orphan-id []
  [:label "Notes" [:textarea {:id "wrapped-orphan"}]])

(defn ok-aria-label-orphan-id []
  [:textarea {:id "named-orphan"
              :aria-label "Notes"}])

(defn ok-switch-children-orphan-id []
  [ui/switch {:id "switch-orphan"} "Dark mode"])

(defn ok-dynamic-props-orphan-id [props]
  [:textarea (merge {:id "merged-orphan"} props)])

(defn ok-div-orphan-id []
  [:div {:id "div-orphan"}])

(defn ok-literal-switch-orphan-id []
  [:switch {:id "lone-literal-switch"}])

(defn ok-keyword-for []
  [:div
   [:label {:for :kw-notes} "Notes"]
   [:textarea {:id "kw-notes"}]])

(defn ok-multi-line-labelledby []
  [:div
   [:div {:aria-labelledby "heading
                            ml-notes"}]
   [:textarea {:id "ml-notes"}]])

(defn ok-js-html-for []
  [:div
   [:> "label" #js {:htmlFor "js-notes"} "Notes"]
   [:textarea {:id "js-notes"}]])

(defn ok-id-repeated-in-key []
  [:textarea {:id "keyed-notes"
              :key "keyed-notes"}])

(defn ok-number-for []
  [:div
   [:label {:for 41} "Count"]
   [:textarea {:id "41"}]])

(defn ok-markup-string-label []
  [:div
   [:div {:dangerouslySetInnerHTML {:__html "<label for=\"html-notes\">Notes</label>"}}]
   [:textarea {:id "html-notes"}]])

(defn ok-id-from-quoted-symbol []
  [:textarea {:id "quoted-notes"}])

(defn ok-id-from-long-quoted-symbol []
  [:textarea {:id "long-quoted-notes"}])

(defn ok-id-from-syntax-quoted-symbol []
  [:textarea {:id "syntax-quoted-notes"}])
