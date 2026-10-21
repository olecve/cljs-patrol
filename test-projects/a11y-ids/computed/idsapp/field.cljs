(ns idsapp.field)

(defn notes-label [prefix]
  [:label {:for (str prefix "-notes")} "Notes"])

(defn ok-id-built-by-computed-for []
  [:textarea {:id "search-notes"}])

(defn bad-id-no-literal-contains []
  [:textarea {:id "zzqx"}])

(defn char-label [n]
  [:label {:for (str \k n)} "K"])

(defn ok-id-built-from-char []
  [:textarea {:id "k7"}])

(defn quoted-label []
  [:label {:for (name 'quoted-notes)} "Notes"])

(defn ok-id-from-quoted-symbol []
  [:textarea {:id "quoted-notes"}])
