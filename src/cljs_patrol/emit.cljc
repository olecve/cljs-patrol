(ns cljs-patrol.emit
  "Small cljc hiccup-to-HTML emitter used by the HTML reporter.
  Covers the subset needed here: tag + optional `.class` shorthand,
  optional attrs map, string/nil/sequential/vector children, and a
  `raw` wrapper for pre-rendered content (CSS, JS) that must not be
  HTML-escaped."
  (:require [clojure.string :as str]))

(defrecord Raw [s])

(defn raw
  "Wrap `s` so `emit` inlines it verbatim (no HTML escaping)."
  [s]
  (->Raw s))

(def ^:private void-elements
  #{"area" "base" "br" "col" "embed" "hr" "img" "input"
    "link" "meta" "param" "source" "track" "wbr"})

(defn- parse-tag
  "Split `tag` (`:div.a.b` or `:span`) into its tag name and any
  `.class` shorthand pieces."
  [tag]
  (let [parts (str/split (name tag) #"\.")
        tag-name (first parts)]
    {:tag (if (empty? tag-name) "div" tag-name)
     :shorthand-classes (vec (rest parts))}))

(defn- escape-text [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn- escape-attr [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "\"" "&quot;")))

(defn- merge-classes [attrs shorthand-classes]
  (if (seq shorthand-classes)
    (let [existing (:class attrs)
          combined (->> (cons existing shorthand-classes)
                        (remove nil?)
                        (remove #(and (string? %) (str/blank? %)))
                        (str/join " "))]
      (assoc attrs :class combined))
    attrs))

(defn- attr-str [attrs shorthand-classes]
  (let [attrs (merge-classes attrs shorthand-classes)]
    (str/join
     (for [[k v] attrs
           :when (some? v)]
       (str " " (name k) "=\"" (escape-attr (str v)) "\"")))))

(declare emit)

(defn- emit-vector [v]
  (let [[raw-tag & tail] v
        {:keys [tag shorthand-classes]} (parse-tag raw-tag)
        [attrs children] (if (map? (first tail))
                           [(first tail) (rest tail)]
                           [{} tail])]
    (if (contains? void-elements tag)
      (str "<" tag (attr-str attrs shorthand-classes) ">")
      (str "<" tag (attr-str attrs shorthand-classes) ">"
           (str/join (map emit children))
           "</" tag ">"))))

(defn emit
  "Render a hiccup form to an HTML string."
  [x]
  (cond
    (nil? x) ""
    (instance? Raw x) (:s x)
    (string? x) (escape-text x)
    (vector? x) (emit-vector x)
    (sequential? x) (str/join (map emit x))
    :else (escape-text (str x))))

(defn emit-document
  "Render a full HTML document. Prepends the HTML5 doctype."
  [body]
  (str "<!DOCTYPE html>\n" (emit body)))
