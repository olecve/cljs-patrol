(ns cljs-patrol.dev.mutate.error
  "The error that stops a run before it starts, or stops it early, with a message for the person who ran it.")

(defn setup-error [message]
  (ex-info message {::setup true}))
