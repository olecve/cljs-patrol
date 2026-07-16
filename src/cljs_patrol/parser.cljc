(ns cljs-patrol.parser
  "Shared AST utilities and generic walker for ClojureScript static analysis."
  (:require
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn- eprintln [& args]
  #?(:clj (binding [*out* *err*] (apply println args))
     :cljs (.error js/console (apply str (interpose " " args)))))

(defn distinct-by
  "Return a collection with duplicates removed, using key-fn to determine identity."
  [key-fn coll]
  (->> coll
       (map (juxt key-fn identity))
       (into {})
       vals))

(defn raw
  "Return the raw source string of a node, preserving :: prefixes and aliases."
  [zloc]
  (n/string (z/node zloc)))

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
    (let [sexpr-value (try (z/sexpr zloc)
                           (catch #?(:clj Exception :cljs :default) _ nil))]
      (when (symbol? sexpr-value) (name sexpr-value)))))

(defn position-row
  "Return the line number of zloc, or 0 on error."
  [zloc]
  (try (first (z/position zloc))
       (catch #?(:clj Exception :cljs :default) _ 0)))

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
                              :refers {}})

(defn- find-ns-info
  "Find and parse the ns form from a rewrite-clj zip.
  Walks top-level siblings until the ns form is found."
  [zloc]
  (loop [loc zloc]
    (when (and loc (not (z/end? loc)))
      (if (and (= :list (z/tag loc))
               (= "ns" (sym-name (z/down loc))))
        (try (parse-ns-form (z/sexpr loc))
             (catch #?(:clj Exception :cljs :default) _ empty-ns-info))
        (recur (z/right loc))))))

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
  (let [all-handlers (map group/parse-handlers enabled-groups)]
    {:handle-list (distinct (keep :handle-list all-handlers))
     :handle-vector (distinct (keep :handle-vector all-handlers))
     :handle-token (distinct (keep :handle-token all-handlers))}))

(defn- call-handlers [handlers tag loc ns-info file]
  (let [fns (cond
              (#{:list :fn} tag) (:handle-list handlers)
              (= :vector tag) (:handle-vector handlers)
              (= :token tag) (:handle-token handlers))]
    (reduce (fn [acc handler]
              (merge-result acc (handler loc ns-info file)))
            empty-result
            fns)))

(defn- file-extension [path]
  (let [dot (str/last-index-of path ".")]
    (when (and dot (pos? dot)) (subs path dot))))

(defn- groups-for-extension
  "Return the subset of enabled-groups that opt into the given file extension."
  [enabled-groups ext]
  (filter #(contains? (group/file-extensions %) ext) enabled-groups))

(defn analyze-file
  "Parse a single .cljs/.cljc file and return {:declarations :usages :dynamic-sites}.
  Only groups whose `file-extensions` include this file's extension are invoked."
  [file-path enabled-groups]
  (let [applicable (groups-for-extension enabled-groups (file-extension file-path))]
    (when (seq applicable)
      (let [zloc (try (z/of-string (fs/slurp-file file-path) {:track-position? true})
                      (catch #?(:clj Exception :cljs :default) e
                        (eprintln "WARN: could not parse" file-path ":"
                                  #?(:clj (.getMessage e) :cljs (.-message e)))
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

(defn find-source-files
  "Recursively find all .cljs and .cljc files under root-dir."
  [root-dir]
  (fs/list-source-files root-dir))

(defn analyze-project
  "Analyze all ClojureScript source files under root-dir using enabled-groups.
  Returns {:declarations :usages :dynamic-sites} across all files."
  [root-dir enabled-groups]
  (let [files (fs/list-source-files root-dir)]
    (eprintln (str "Analyzing " (count files) " files under " root-dir " ..."))
    (let [results (keep #(analyze-file % enabled-groups) files)]
      {:declarations (mapcat :declarations results)
       :usages (mapcat :usages results)
       :dynamic-sites (mapcat :dynamic-sites results)})))
