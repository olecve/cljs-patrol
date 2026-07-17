(ns cljs-patrol.groups.reagent
  "Reagent rule group: detects suboptimal patterns in Reagent hiccup templates."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.spade :as spade]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private snippet-max-length 120)

(defn- source-snippet [loc]
  (let [raw (try (z/string loc) (catch Exception _ ""))
        collapsed (str/replace raw #"\s+" " ")]
    (if (> (count collapsed) snippet-max-length)
      (str (subs collapsed 0 (- snippet-max-length 3)) "...")
      collapsed)))

(defn- hiccup-head-loc?
  "True when `loc` is a token that plausibly heads a Hiccup vector:
  a keyword that parses as a Hiccup tag, or any symbol (bare or
  namespaced) — Reagent components appear here as ordinary refs."
  [loc]
  (and loc
       (= :token (z/tag loc))
       (or (some? (hiccup/parse-tag (parser/raw loc)))
           (some? (parser/sym-name loc)))))

(defn- data-vector?
  "True when the vector's second element is also a keyword literal —
  the shape [:ns :key …] and similar are almost always data/schema
  structures, not Hiccup elements."
  [vec-loc]
  (let [second-loc (some-> vec-loc z/down z/right)]
    (and second-loc (parser/kw-node? second-loc))))

(defn- redundant-into?
  "Match `(into [HEAD …] EXPR …)` where HEAD looks like a Hiccup head
  and the vector doesn't obviously carry data-shape keywords."
  [loc]
  (when (= "into" (parser/sym-name (z/down loc)))
    (let [vec-loc (some-> loc z/down z/right)
          more-loc (some-> vec-loc z/right)]
      (and vec-loc
           more-loc
           (= :vector (z/tag vec-loc))
           (hiccup-head-loc? (z/down vec-loc))
           (not (data-vector? vec-loc))))))

(defn- head-identifier
  "Return the head token's raw text as a keyword (for keyword tags) or symbol."
  [head-loc]
  (let [raw (parser/raw head-loc)]
    (if-let [tag (hiccup/parse-tag raw)]
      tag
      (symbol raw))))

(defn- redundant-into-finding [loc file]
  (let [head-loc (some-> loc z/down z/right z/down)
        [row col] (try (z/position loc) (catch Exception _ [0 1]))]
    {:kw (head-identifier head-loc)
     :type :redundant-into-hiccup
     :form (source-snippet loc)
     :file file
     :row row
     :col col}))

(def ^:private spade-handle-list
  (get (group/parse-handlers spade/group) :handle-list))

(defn- handle-into
  "Emit a :redundant-into-hiccup finding when `loc` matches the anti-pattern."
  [loc _ns-info file]
  (when (and (not (hiccup/inside-quoted-form? loc))
             (not (hiccup/inside-style-decl? loc))
             (not (hiccup/inside-ns-form? loc))
             (redundant-into? loc))
    {:decls [(redundant-into-finding loc file)]
     :usages []
     :dynamics []}))

(defn- analyze* [{:keys [declarations usages]}]
  (let [style-decls (filter #(= :defclass (:type %)) declarations)
        style-calls (filter #(= :style-call (:type %)) usages)
        usages-by-kw (group-by :kw style-calls)
        defclass-as-sole-attr (for [decl style-decls
                                    :let [uses (get usages-by-kw (:kw decl))]
                                    :when (seq uses)
                                    :when (every? #(= :class-only-map (:context %)) uses)]
                                decl)
        redundant-into (filter #(= :redundant-into-hiccup (:type %)) declarations)]
    {:defclass-as-sole-attr (vec defclass-as-sole-attr)
     :redundant-into-hiccup (vec redundant-into)}))

(defn- summary-lines* [{:keys [defclass-as-sole-attr redundant-into-hiccup]}]
  [["defclass as sole attr:" (count defclass-as-sole-attr)]
   ["Redundant into-hiccup:" (count redundant-into-hiccup)]])

(defn- failed?* [_] false)

(defrecord ReagentGroup []
  group/RuleGroup
  (group-id [_] :reagent)
  (group-name [_] "Reagent")
  (parse-handlers [_] {:handle-list [spade-handle-list handle-into]})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:defclass-as-sole-attr
     "Declared with defclass but every usage is {:class (style-fn)}. Use defattrs instead to avoid the :class wrapper."
     :redundant-into-hiccup
     "Reagent inlines top-level seqs in Hiccup — `(into [:span] children)` and `[:span children]` render identically. Drop the `into` wrapper and let the child expression sit directly inside the vector."})
  (rule->tier [_]
    {:defclass-as-sole-attr :deprecations
     :redundant-into-hiccup :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->ReagentGroup))
