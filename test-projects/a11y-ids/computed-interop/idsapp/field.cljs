(ns idsapp.field)

(defn point-label! [el n]
  (set! (.. el -htmlFor) (str "dq" n)))

(defn ok-id-built-by-interop-property []
  [:textarea {:id "dq1"}])

(defn bad-id-no-literal-contains []
  [:textarea {:id "zzwx"}])
