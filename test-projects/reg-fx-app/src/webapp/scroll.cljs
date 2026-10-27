(ns webapp.scroll)

(defn scroll-to! [id]
  (some-> (js/document.getElementById id) .scrollIntoView))

(defn shared! [value]
  (js/console.log value))
