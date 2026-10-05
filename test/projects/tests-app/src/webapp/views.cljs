(ns webapp.views)

(defn handler []
  ;; a production namespace, so the rule says nothing
  @#'other.ns/seen-ids)

(defn typed-handler [^js el]
  ;; a production namespace: the hint can be load-bearing here
  (.-value el))
