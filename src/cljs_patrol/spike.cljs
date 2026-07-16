(ns cljs-patrol.spike
  "Phase-0 smoke test: load rewrite-clj under Node and walk one snippet."
  (:require
   [rewrite-clj.zip :as z]))

(def snippet
  "(ns example.hello
     (:require [reagent.core :as r]))

   (defn greeting [name]
     [:div {:on-click #(js/alert name)}
      \"Hello, \" name])")

(defn -main [& _args]
  (let [zloc (z/of-string snippet {:track-position? true})]
    (println "rewrite-clj loaded ✓")
    (println "tag =" (z/tag zloc)
             "sexpr =" (pr-str (z/sexpr zloc)))
    (loop [loc zloc, tags []]
      (if (z/end? loc)
        (do (println "walked" (count tags) "nodes; tag histogram:"
                     (pr-str (frequencies tags)))
            (println "smoke test passed."))
        (recur (z/next loc) (conj tags (z/tag loc)))))))
