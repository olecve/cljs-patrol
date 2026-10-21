(ns idsapp.field)

(defn js-label [n]
  [:> "label" #js {"htmlFor" (str "jq" n)} "Q"])

(defn ok-id-built-by-js-for []
  [:textarea {:id "jq1"}])

(defn bad-id-no-literal-contains []
  [:textarea {:id "zzwx"}])
