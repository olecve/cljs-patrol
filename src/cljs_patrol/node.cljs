(ns cljs-patrol.node
  "Node entry point. Forwards process argv to cljs-patrol.core/-main."
  (:require
   [cljs-patrol.core :as core]))

(defn -main [& args]
  (apply core/-main args))
