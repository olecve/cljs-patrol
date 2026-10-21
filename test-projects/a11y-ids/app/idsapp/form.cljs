(ns idsapp.form)

(defn ok-labelled-from-shared-dir []
  [:textarea {:id "cross-dir-notes"}])

(defn bad-orphan-id []
  [:textarea {:id "lonely-notes"}])
