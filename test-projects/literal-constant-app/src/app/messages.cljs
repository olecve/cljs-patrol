(ns app.messages)

(def file-failed-message "We couldn't read this file")

(def generic-failed-message
  "Heading for any other failure."
  "Smart search failed")

(def ^:private support-email "support@example.com")

(def view-results-label "View Results")

(def completed-message (str "Smart Search " "completed"))

#?(:cljs (def platform-label "Browser"))

(defn completed-description [working-title]
  (str "Search case \"" (or working-title "Untitled") "\" is ready for review"))

(def js-config #js {:label "View Results"})

(def label-pattern #"View Results")

(goog-define api-host "localhost")

(def legacy-label "Old" #_"Older")

(def duplicate-label "Cljs")

(def ^:dynamic *greeting* "Hello")

(def unbound)
