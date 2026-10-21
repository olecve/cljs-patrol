(ns idsapp.field)

(defn point-label! [el n]
  (set! (.-htmlFor el) (str "pq" n)))

(defn ok-id-built-by-dom-property []
  [:textarea {:id "pq1"}])

(defn bad-id-no-literal-contains []
  [:textarea {:id "zzwx"}])
