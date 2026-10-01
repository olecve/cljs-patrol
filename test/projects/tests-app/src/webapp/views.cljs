(ns webapp.views)

(defn handler []
  ;; a production namespace, so the rule says nothing
  @#'other.ns/seen-ids)
