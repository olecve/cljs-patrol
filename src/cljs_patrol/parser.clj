(ns cljs-patrol.parser
  "Shared AST utilities and generic walker for ClojureScript static analysis."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn distinct-by [key-fn coll]
  (->> coll
       (map (juxt key-fn identity))
       (into {})
       vals))

(defn raw
  "Return the raw source string of a node, preserving :: prefixes and aliases."
  [zloc]
  (n/string (z/node zloc)))

(defn normalize-form
  "Collapse a form's source to one line, so reformatting it does not change the text.

  Both a finding's `:form` and the baseline identity built from it go through this, and
  they have to agree character for character or a baseline stops matching what produced
  it. Whitespace inside collapses to one space, and the space a line break leaves against
  a bracket goes entirely, so a form wrapped across lines with its closing bracket on one
  of its own reads the same as the one-line form it was before someone reformatted it."
  [source]
  (-> (str source)
      str/trim
      (str/replace #"\s+" " ")
      (str/replace #"([(\[{]) " "$1")
      (str/replace #" ([)\]}])" "$1")))

(defn kw-node?
  "True if the node is a keyword token (including :: prefixed ones)."
  [zloc]
  (and zloc
       (= :token (z/tag zloc))
       (str/starts-with? (raw zloc) ":")))

(defn sym-name
  "Return the name string of a symbol token, or nil."
  [zloc]
  (when (and zloc (= :token (z/tag zloc)))
    (let [sexpr-value (try (z/sexpr zloc) (catch Exception _ nil))]
      (when (symbol? sexpr-value) (name sexpr-value)))))

(defn declared-name-loc
  "Return the zloc of the symbol a `def`-like form names, or nil.

  Metadata stacks — `(defclass ^:private ^:const x …)` nests one `:meta` node inside another —
  so the name is whatever is left once every layer is peeled off. Reading the slot after the
  operator directly finds a `:meta` node instead, and skips the whole form."
  [list-loc]
  (loop [loc (some-> list-loc z/down z/right)]
    (if (= :meta (some-> loc z/tag))
      (recur (some-> loc z/down z/rightmost))
      loc)))

(defn top-level-name
  "Return what the top-level form holding loc defines, or nil.

  The var a `def`-like form names, or the keyword a registration such as
  `(reg-event-fx ::saved …)` is keyed by."
  [loc]
  (let [top (last (take-while #(and % (not= :forms (z/tag %))) (iterate z/up loc)))
        head (some-> top z/down sym-name)]
    (when (and head (= :list (z/tag top)))
      (if (str/starts-with? head "def")
        (some-> (declared-name-loc top) sym-name)
        (let [id (some-> top z/down z/right)]
          (when (kw-node? id) (raw id)))))))

(defn child-locs
  "Return the locs of loc's children, in order, or nil when it has none."
  [loc]
  (when-let [first-child (some-> loc z/down)]
    (take-while some? (iterate #(some-> % z/right) first-child))))

(defn right-siblings
  "Return the locs to the right of loc, loc itself excluded."
  [loc]
  (take-while some? (rest (iterate #(some-> % z/right) loc))))

(defn position-row
  "Return the line number of zloc, or 0 on error."
  [zloc]
  (try (first (z/position zloc)) (catch Exception _ 0)))

(defn resolve-kw
  "Resolve a raw keyword string to a fully-qualified Clojure keyword.
  - ::alias/name  -> :full.ns/name  (via require alias map)
  - ::local-name  -> :current.ns/name
  - :ns/name      -> :ns/name       (unchanged)
  - :plain        -> :plain         (unchanged)
  Returns nil if the alias is unknown."
  [kw-str ns-name aliases]
  (cond
    (and (str/starts-with? kw-str "::") (str/includes? kw-str "/"))
    (let [after (subs kw-str 2)
          slash (str/index-of after "/")
          alias-part (subs after 0 slash)
          name-part (subs after (inc slash))
          full-ns (get aliases alias-part)]
      (when full-ns (keyword full-ns name-part)))

    (str/starts-with? kw-str "::")
    (keyword ns-name (subs kw-str 2))

    :else
    (keyword (subs kw-str 1))))

(defn resolve-sym
  "Resolve a raw symbol string to a fully-qualified keyword.
  - alias/name -> :full.ns/name (via require alias map)
  - name -> :current.ns/name"
  [sym-str ns-name aliases]
  (if (str/includes? sym-str "/")
    (let [slash (str/index-of sym-str "/")
          alias-part (subs sym-str 0 slash)
          name-part (subs sym-str (inc slash))
          full-ns (get aliases alias-part)]
      (when full-ns (keyword full-ns name-part)))
    (keyword ns-name sym-str)))

(defn extract-kw-from-vector
  "Return {:kw resolved-kw :dynamic? bool} based on the first element of vec-zloc.

  Sets :dynamic? true when the first element is not a literal keyword."
  [vec-zloc ns-name aliases]
  (when (= :vector (z/tag vec-zloc))
    (when-let [first-elem (z/down vec-zloc)]
      (if (kw-node? first-elem)
        {:kw (resolve-kw (raw first-elem) ns-name aliases)
         :dynamic? false}
        {:kw nil
         :dynamic? true}))))

(defn- parse-require [req-vec]
  (when (vector? req-vec)
    (when-let [ns-sym (first req-vec)]
      (loop [pairs (rest req-vec)
             as-alias nil
             refers []]
        (if (< (count pairs) 2)
          {:full-ns (str ns-sym)
           :as as-alias
           :refers refers}
          (let [k (first pairs)
                v (second pairs)]
            (cond
              (= :as k) (recur (drop 2 pairs) (str v) refers)
              (and (= :refer k) (sequential? v))
              (recur (drop 2 pairs) as-alias (into refers (map str) v))
              :else (recur (drop 2 pairs) as-alias refers))))))))

(defn- parse-ns-form
  "Return {:ns-name str :aliases {alias-str full-ns-str} :refers {refer-name-str full-ns-str}}."
  [ns-sexpr]
  (let [ns-name (str (second ns-sexpr))
        requires (for [clause (rest ns-sexpr)
                       :when (and (seq? clause) (= :require (first clause)))
                       req (rest clause)
                       :let [parsed (parse-require req)]
                       :when parsed]
                   parsed)
        aliases (into {}
                      (keep (fn [{:keys [full-ns as]}]
                              (when as [as full-ns])))
                      requires)
        refers (into {}
                     (mapcat (fn [{:keys [full-ns refers]}]
                               (map (fn [r] [r full-ns]) refers)))
                     requires)]
    {:ns-name ns-name
     :aliases aliases
     :refers refers}))

(def ^:private empty-ns-info {:ns-name "unknown"
                              :aliases {}
                              :refers {}
                              :shadowed-core :all})

(defn- shadowed-by-ns-form
  "Return the core names an ns form may shadow, or :all when it may shadow any.

  Read from the nodes rather than the parsed form, so a clause behind a reader
  conditional counts too. Every symbol the clauses write is a name they may exclude,
  rename or refer over core's, whichever clause it sits in, except the alias after an
  `:as` or `:as-alias`: `[clojure.set :as set]` names a namespace, and `set` stays core's.
  A `:use`, a `:refer :all` or a `:refer-clojure :only` can take a core name without
  writing it, so any one of them takes them all."
  [ns-node]
  (let [tokens (->> (tree-seq n/inner? n/children ns-node)
                    (filter #(= :token (n/tag %)))
                    (map #(try (n/sexpr %) (catch Exception _ nil))))]
    (if (some #{:use :all :only} tokens)
      :all
      (->> (map vector (cons nil tokens) tokens)
           (remove (fn [[previous _]] (contains? #{:as :as-alias} previous)))
           (map second)
           (filter symbol?)
           (map name)
           set))))

(def ^:private namespace-changing-heads
  "Top-level calls that switch namespace, refer names, or define vars no form names after the ns form has run."
  #{"ns" "in-ns" "require" "use" "refer" "refer-clojure" "load" "intern"})

(defn- list-head-name
  "Return the name of the symbol heading list node, or nil."
  [node]
  (when (= :list (n/tag node))
    (let [head (first (remove n/whitespace-or-comment? (n/children node)))
          value (when (= :token (some-> head n/tag)) (try (n/sexpr head) (catch Exception _ nil)))]
      (when (symbol? value) (name value)))))

(defn- written-name
  "Return the name of the symbol node writes, under any metadata, or nil."
  [node]
  (let [node (if (= :meta (some-> node n/tag)) (last (n/children node)) node)
        value (when (= :token (some-> node n/tag)) (try (n/sexpr node) (catch Exception _ nil)))]
    (when (symbol? value) (name value))))

(defn- defined-names
  "Return the vars a list node defines: a `def`-headed form's, a `declare`'s, a `defprotocol`'s methods."
  [node]
  (let [head (list-head-name node)
        arguments (rest (remove n/whitespace-or-comment? (n/children node)))]
    (concat
     (when (= "declare" head)
       (keep written-name arguments))
     (when (= "defprotocol" head)
       (keep list-head-name arguments))
     (when (some-> head (str/starts-with? "def"))
       (keep written-name [(first arguments)])))))

(defn- shadowed-by-other-forms
  "Return the core names the top-level forms besides the ns form may shadow, or :all.

  A second ns form, an `in-ns` or a top-level `require` can bring in any name. Every var
  the file defines has its name shadowed too, at any depth: the rules settle a top-level
  `def` the same way, and read none in a `do`, a reader conditional or a function body,
  nor a `declare` or a protocol's methods, which replace core's var all the same."
  [top-level-nodes]
  (let [inner-lists (fn [node]
                      (if (= :reader-macro (n/tag node))
                        (filter #(= :list (n/tag %)) (tree-seq n/inner? n/children node))
                        [node]))
        lists (mapcat inner-lists top-level-nodes)]
    (if (some #(contains? namespace-changing-heads (list-head-name %)) lists)
      :all
      (->> (mapcat #(tree-seq n/inner? n/children %) top-level-nodes)
           (filter #(= :list (n/tag %)))
           (mapcat defined-names)
           set))))

(defn- shadowed-core
  "Return the core names a file may shadow, or :all when it may shadow any."
  [ns-node other-nodes]
  (let [by-ns (shadowed-by-ns-form ns-node)
        by-others (shadowed-by-other-forms other-nodes)]
    (if (or (= :all by-ns) (= :all by-others))
      :all
      (into by-ns by-others))))

(defn- find-ns-info
  "Find and parse the ns form from a rewrite-clj zip.

  z/of-file positions at the first top-level form (not a :forms wrapper).
  Walk top-level siblings with z/right until the ns form is found."
  [zloc]
  (let [top-level (take-while #(and % (not (z/end? %))) (iterate z/right zloc))]
    (when-let [ns-loc (first (filter #(and (= :list (z/tag %)) (= "ns" (sym-name (z/down %)))) top-level))]
      (try (assoc (parse-ns-form (z/sexpr ns-loc))
                  :shadowed-core (shadowed-core (z/node ns-loc)
                                                (->> top-level
                                                     (remove #(identical? (z/node %) (z/node ns-loc)))
                                                     (map z/node))))
           (catch Exception _ empty-ns-info)))))

(def ^:private empty-result {:decls []
                             :usages []
                             :dynamics []})

(defn- merge-result [acc node-result]
  (if node-result
    {:decls (into (:decls acc) (:decls node-result))
     :usages (into (:usages acc) (:usages node-result))
     :dynamics (into (:dynamics acc) (:dynamics node-result))}
    acc))

(defn- collect-handlers
  "Collect unique handler functions from all enabled groups, keyed by handler type."
  [enabled-groups]
  (let [all-handlers (map group/parse-handlers enabled-groups)
        gather (fn [k]
                 (->> all-handlers
                      (mapcat #(let [v (get % k)]
                                 (cond
                                   (nil? v) nil
                                   (sequential? v) v
                                   :else [v])))
                      (keep identity)
                      distinct))]
    {:handle-list (gather :handle-list)
     :handle-vector (gather :handle-vector)
     :handle-token (gather :handle-token)}))

(defn- call-handlers [handlers tag loc ns-info file]
  (let [fns (cond
              (#{:list :fn} tag) (:handle-list handlers)
              (= :vector tag) (:handle-vector handlers)
              (= :token tag) (:handle-token handlers))]
    (reduce (fn [acc handler]
              (merge-result acc (handler loc ns-info file)))
            empty-result
            fns)))

(defn- file-extension [^String path]
  (let [dot (.lastIndexOf path ".")]
    (when (pos? dot) (subs path dot))))

(defn- groups-for-extension [enabled-groups ext]
  (filter #(contains? (group/file-extensions %) ext) enabled-groups))

(defn analyze-file
  "Parse a single source file and return {:declarations :usages :dynamic-sites}.
  Only groups whose `file-extensions` include this file's extension are invoked."
  [file enabled-groups]
  (let [file-path (str file)
        applicable (groups-for-extension enabled-groups (file-extension file-path))]
    (when (seq applicable)
      (let [zloc (try (z/of-file file {:track-position? true})
                      (catch Exception e
                        (binding [*out* *err*]
                          (println "WARN: could not parse" file-path ":" (.getMessage e)))
                        nil))]
        (when zloc
          (let [ns-info (or (find-ns-info zloc) empty-ns-info)
                handlers (collect-handlers applicable)]
            (loop [loc zloc
                   result empty-result]
              (if (z/end? loc)
                {:declarations (:decls result)
                 :usages (:usages result)
                 :dynamic-sites (:dynamics result)}
                (let [tag (z/tag loc)]
                  (recur (z/next loc)
                         (merge-result result (call-handlers handlers tag loc ns-info file-path))))))))))))

(defn enabled-extensions
  "Union of the extensions `enabled-groups` declare, so a file is opened only when some enabled group can read it."
  [enabled-groups]
  (into #{} (mapcat group/file-extensions) enabled-groups))

(defn analyze-project
  "Analyze all source files under root-dir using enabled-groups.
  Returns {:declarations :usages :dynamic-sites} across all files."
  [root-dir enabled-groups]
  (let [files (fs/list-source-files root-dir (enabled-extensions enabled-groups))]
    (binding [*out* *err*]
      (println (str "Analyzing " (count files) " files under " root-dir " ...")))
    (let [results (keep #(analyze-file % enabled-groups) files)]
      {:declarations (mapcat :declarations results)
       :usages (mapcat :usages results)
       :dynamic-sites (mapcat :dynamic-sites results)})))
