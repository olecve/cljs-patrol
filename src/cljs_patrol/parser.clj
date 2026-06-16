(ns cljs-patrol.parser
  "Shared AST utilities and generic walker for ClojureScript static analysis."
  (:require
   [cljs-patrol.group :as group]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   [java.io File]))

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
    (let [sexpr-value (try (z/sexpr zloc) (catch Exception _ nil))]
      (when (symbol? sexpr-value) (name sexpr-value)))))

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

(defn- parse-require-alias
  "Extract [alias-str full-ns-str] from a require vector like [full.ns :as alias], or nil."
  [req-vec]
  (when (vector? req-vec)
    (let [ns-sym (first req-vec)
          pairs (rest req-vec)]
      (when ns-sym
        (loop [remaining-pairs pairs]
          (when (>= (count remaining-pairs) 2)
            (if (= :as (first remaining-pairs))
              [(str (second remaining-pairs)) (str ns-sym)]
              (recur (rest remaining-pairs)))))))))

(defn- parse-ns-form
  "Extract {:ns-name str :aliases {alias-str full-ns-str}} from a (ns ...) sexpr."
  [ns-sexpr]
  (let [ns-name (str (second ns-sexpr))
        aliases (into {}
                      (for [clause (rest ns-sexpr)
                            :when (and (seq? clause) (= :require (first clause)))
                            req (rest clause)
                            :let [pair (parse-require-alias req)]
                            :when pair]
                        pair))]
    {:ns-name ns-name
     :aliases aliases}))

(defn- find-ns-info
  "Find and parse the ns form from a rewrite-clj zip.
  z/of-file positions at the first top-level form (not a :forms wrapper).
  Walk top-level siblings with z/right until the ns form is found."
  [zloc]
  (loop [loc zloc]
    (when (and loc (not (z/end? loc)))
      (if (and (= :list (z/tag loc))
               (= "ns" (sym-name (z/down loc))))
        (try (parse-ns-form (z/sexpr loc))
             (catch Exception _ {:ns-name "unknown"
                                 :aliases {}}))
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

(defn- call-handlers [handlers tag loc ns-name aliases file]
  (let [fns (cond
              (#{:list :fn} tag) (:handle-list handlers)
              (= :vector tag) (:handle-vector handlers)
              (= :token tag) (:handle-token handlers))]
    (reduce (fn [acc handler]
              (merge-result acc (handler loc ns-name aliases file)))
            empty-result
            fns)))

(defn- file-extension [^String path]
  (let [dot (.lastIndexOf path ".")]
    (when (pos? dot) (subs path dot))))

(defn- groups-for-extension
  "Return the subset of enabled-groups that opt into the given file extension."
  [enabled-groups ext]
  (filter #(contains? (group/file-extensions %) ext) enabled-groups))

(defn analyze-file
  "Parse a single .cljs/.cljc file and return {:declarations :usages :dynamic-sites}.
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
          (let [{:keys [aliases ns-name]
                 :or {ns-name "unknown"
                      aliases {}}}
                (or (find-ns-info zloc) {:ns-name "unknown"
                                         :aliases {}})
                handlers (collect-handlers applicable)]
            (loop [loc zloc
                   result empty-result]
              (if (z/end? loc)
                {:declarations (:decls result)
                 :usages (:usages result)
                 :dynamic-sites (:dynamics result)}
                (let [tag (z/tag loc)]
                  (recur (z/next loc)
                         (merge-result result (call-handlers handlers tag loc ns-name aliases file-path))))))))))))

(defn find-source-files
  "Recursively find all .cljs and .cljc files under root-dir."
  [root-dir]
  (->> (file-seq (io/file root-dir))
       (filter (fn [^File f]
                 (and (.isFile f)
                      (let [file-name (.getName f)]
                        (or (str/ends-with? file-name ".cljs")
                            (str/ends-with? file-name ".cljc"))))))))

(defn analyze-project
  "Analyze all ClojureScript source files under root-dir using enabled-groups.
  Returns {:declarations :usages :dynamic-sites} across all files."
  [root-dir enabled-groups]
  (let [files (find-source-files root-dir)]
    (binding [*out* *err*]
      (println (str "Analyzing " (count files) " files under " root-dir " ...")))
    (let [results (keep #(analyze-file % enabled-groups) files)]
      {:declarations (mapcat :declarations results)
       :usages (mapcat :usages results)
       :dynamic-sites (mapcat :dynamic-sites results)})))
