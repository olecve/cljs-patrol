(ns app.core)

(defn summarize
  "A summary line with no terminator
  and a second line below it."
  [x]
  x)

(def points
  [{:x 0
    :y 0} {:x 10
           :y 0}])

(defn view []
  [:img {:src "logo.png"}])
