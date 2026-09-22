(ns cljs-patrol.style-blocks
  "Shared reading of Spade defclass/defattrs bodies.
  A declaration holds one base style map plus nested selector vectors, each carrying a selector
  and a style map of its own, to any depth. Every rule over style declarations needs the same two
  views of that shape — the maps, and the selectors above them — so they are read in one place."
  (:require
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def style-decl-fns #{"defclass" "defattrs"})

(defn style-decl? [list-loc]
  (contains? style-decl-fns (parser/sym-name (z/down list-loc))))

(defn children [loc]
  (take-while some? (iterate z/right (z/down loc))))

(defn map-key-locs [map-loc]
  (take-nth 2 (children map-loc)))

(defn selector-head
  "Zlocs of a nested selector vector up to its style map.
  Garden reads those as the selector; the map and anything after it is the body."
  [vector-loc]
  (take-while #(not= :map (z/tag %)) (children vector-loc)))

(defn selector-label [vector-loc]
  (str/join " " (map parser/raw (selector-head vector-loc))))

(defn selector-block?
  "True when the vector carries a style map of its own.
  That is what separates a selector block from one of Garden's value vectors, such as the
  `[[0 :auto]]` in `{:margin [[0 :auto]]}`."
  [vector-loc]
  (boolean (some #(= :map (z/tag %)) (children vector-loc))))

(defn nested-vectors
  "Every vector under `loc`, depth-first, without descending into style maps."
  [loc]
  (mapcat (fn [child]
            (case (z/tag child)
              :map nil
              :vector (cons child (nested-vectors child))
              (nested-vectors child)))
          (children loc)))

(defn selector-blocks [loc]
  (filter selector-block? (nested-vectors loc)))

(defn style-maps
  "Every map literal Spade reads as a style body, tagged with the selector path above it.
  The base map's path is the empty string. Only literals nested directly in the declaration or
  in a selector vector are collected — a map a call produces, `(merge …)` or `(case …)`, is not
  something this can read."
  ([loc] (style-maps loc ""))
  ([loc selector]
   (mapcat (fn [child]
             (case (z/tag child)
               :map [{:map-loc child
                      :selector selector}]
               :vector (style-maps child (str/trim (str selector " " (selector-label child))))
               nil))
           (children loc))))
