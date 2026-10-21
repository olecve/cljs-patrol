(ns idsapp.ids-labels)

(def constant-id "constant-id")

(defn cross-file-label []
  [:label {:for "cross-file-id"} "Notes"])

(def quoted-notes-id (name 'quoted-notes))

(def long-quoted-notes-id (name (quote long-quoted-notes)))

(def syntax-quoted-notes-id (name `syntax-quoted-notes))
