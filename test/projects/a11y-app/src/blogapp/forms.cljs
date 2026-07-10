(ns blogapp.forms
  (:require
   [blogapp.ui :as ui :refer [textarea]]))

;; Native <textarea>. No aria-label / aria-labelledby -> flagged.
(defn bad-native-placeholder-only []
  [:textarea {:placeholder "Search materials"
              :rows 4}])

;; Native <textarea> with no props at all -> also flagged.
(defn bad-native-no-attrs []
  [:textarea])

;; Native <textarea> with :aria-label -> OK.
(defn ok-native-aria-label []
  [:textarea {:aria-label "Search materials"
              :placeholder "Search materials"
              :rows 4}])

;; Native <textarea> with :aria-labelledby -> OK.
(defn ok-native-aria-labelledby []
  [:label {:id "notes-label"} "Notes"]
  [:textarea {:aria-labelledby "notes-label"
              :rows 4}])

;; Native <textarea> with a computed aria-label -> optimistically accepted.
(defn ok-native-dynamic-aria-label [label]
  [:textarea {:aria-label label
              :rows 4}])

;; Aliased wrapper (`[ui/textarea …]`). Flagged only when the project's
;; :component-aliases config maps blogapp.ui/textarea -> :textarea.
(defn bad-wrapper-alias-placeholder-only []
  [ui/textarea {:placeholder "Search materials"
                :rows 4}])

;; Aliased wrapper with :aria-label -> OK even under config.
(defn ok-wrapper-alias-aria-label []
  [ui/textarea {:aria-label "Search materials"
                :rows 4}])

;; :refer'd wrapper (bare `textarea` symbol). Same rule as the aliased case.
(defn bad-wrapper-refer-placeholder-only []
  [textarea {:placeholder "Search materials"
             :rows 4}])
