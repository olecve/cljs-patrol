(ns webapp.util)

(defn merge [a b] (into b a))

(defn assoc [m k v] (cljs.core/assoc m k v))
