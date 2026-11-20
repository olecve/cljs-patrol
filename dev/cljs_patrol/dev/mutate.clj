(ns cljs-patrol.dev.mutate
  "Mutation testing in one warm JVM: break one operator of a namespace at a time, and check its tests notice.

  Each mutant swaps one symbol or literal, such as `when` for `when-not` or `<` for `<=`. The
  mutated source is loaded over the namespace, the test namespaces run, and the original is
  loaded back. A mutant the tests still pass on is a survivor: a branch no test pins."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as t]
   [clojure.tools.cli :as cli]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:gen-class))

(def ^:private swaps
  "Each operator and the one it is replaced with, both ways."
  (let [pairs [['when 'when-not]
               ['if 'if-not]
               ['and 'or]
               ['= 'not=]
               ['< '<=]
               ['> '>=]
               ['seq 'empty?]
               ['filter 'remove]
               ['some? 'nil?]
               ['true? 'false?]
               ['inc 'dec]
               ['first 'last]
               ['every? 'some]]]
    (into {} (mapcat (fn [[a b]] [[a b] [b a]])) pairs)))

(def ^:private literal-swaps
  {true false
   false true})

(defn- head-symbol [loc]
  (when (= :list (z/tag loc))
    (let [head (z/down loc)]
      (when (= :token (some-> head z/tag))
        (let [value (try (z/sexpr head) (catch Exception _ nil))]
          (when (symbol? value) value))))))

(defn- skipped-form?
  "True when the form at `loc` holds no code a test reaches: the ns form, a `comment`, a `#_` or a quoted form."
  [loc]
  (or (contains? #{:uneval :quote :syntax-quote} (z/tag loc))
      (contains? #{'ns 'comment} (head-symbol loc))))

(defn- inside-skipped? [loc]
  (some skipped-form? (take-while some? (iterate z/up loc))))

(defn- top-level-name
  "Return the name the top-level form around `loc` defines, or nil."
  [loc]
  (let [top (last (take-while #(and % (not= :forms (z/tag %))) (iterate z/up loc)))]
    (when (= :list (z/tag top))
      (some-> top z/down z/right (as-> name-loc (when (= :token (z/tag name-loc)) (z/sexpr name-loc)))))))

(defn- replacement
  "Return the token to put in place of the one at `loc`, or nil when no operator swaps it."
  [loc]
  (when (= :token (z/tag loc))
    (let [value (try (z/sexpr loc) (catch Exception _ ::unreadable))]
      (cond
        (and (symbol? value) (contains? swaps value)) (get swaps value)
        (contains? literal-swaps value) (get literal-swaps value)))))

(defn- mutants
  "Return {:row … :col … :from … :to … :def … :source …} for every swap the source of a namespace allows."
  [source]
  (loop [loc (z/of-string source {:track-position? true})
         found []]
    (if (z/end? loc)
      found
      (recur (z/next loc)
             (if-let [to (when-not (inside-skipped? loc) (replacement loc))]
               (let [[row col] (z/position loc)]
                 (conj found {:row row
                              :col col
                              :from (z/string loc)
                              :to (str to)
                              :def (top-level-name loc)
                              :source (z/root-string (z/replace loc (n/token-node to)))}))
               found)))))

(defn- run-tests
  "Run the test namespaces quietly, and return :passed, :failed or :timeout."
  [test-nss timeout-ms]
  (let [run (future (binding [t/*test-out* (java.io.StringWriter.)]
                      (let [{:keys [fail error]} (apply t/run-tests test-nss)]
                        (if (zero? (+ fail error)) :passed :failed))))
        result (deref run timeout-ms :timeout)]
    (when (= :timeout result)
      (future-cancel run))
    result))

(defn- load-source [source]
  (binding [*ns* *ns*]
    (load-string source)))

(defn- source-of
  "Return the source text of the namespace `ns-sym`, found on the classpath."
  [ns-sym]
  (let [path (-> (name ns-sym) (str/replace "-" "_") (str/replace "." "/"))]
    (some #(some-> (io/resource (str path %)) slurp) [".clj" ".cljc"])))

(def ^:private cli-options
  [["-t" "--test NS" "A test namespace to run, given once per namespace"
    :multi true
    :default []
    :update-fn conj
    :parse-fn symbol]
   ["-d" "--def NAME" "Mutate only inside this top-level definition, given once per name"
    :multi true
    :default []
    :update-fn conj
    :parse-fn symbol]
   [nil "--timeout MS" "Milliseconds a test run may take before the mutant counts as killed"
    :default 30000
    :parse-fn parse-long]])

(defn mutate
  "Mutate `target` and report each mutant the tests in `test-nss` still pass on."
  [target test-nss {:keys [defs timeout]}]
  (let [source (source-of target)
        selected (cond->> (mutants source)
                   (seq defs) (filter #(contains? (set defs) (:def %))))]
    (require target)
    (run! require test-nss)
    (when-not (= :passed (run-tests test-nss timeout))
      (throw (ex-info "The tests fail before any mutation; fix them first." {:tests test-nss})))
    (println (format "%d mutants of %s against %s" (count selected) target (str/join ", " test-nss)))
    (let [started (System/nanoTime)
          results (doall
                   (for [mutant selected]
                     (let [outcome (try
                                     (load-source (:source mutant))
                                     (run-tests test-nss timeout)
                                     (catch Throwable _ :failed)
                                     (finally (load-source source)))]
                       (print (case outcome :passed "S" :timeout "T" "."))
                       (flush)
                       (assoc mutant :outcome outcome))))
          survivors (filter #(= :passed (:outcome %)) results)
          seconds (/ (- (System/nanoTime) started) 1e9)]
      (println)
      (doseq [{:keys [row col from to def]} survivors]
        (println (format "  survived  %d:%d  %s -> %s  in %s" row col from to def)))
      (println (format "%d of %d survived, %.1f s (%.2f s per mutant)"
                       (count survivors) (count results) seconds (/ seconds (max 1 (count results)))))
      survivors)))

(def ^:private usage "Usage: clojure -M:mutate TARGET-NS -t TEST-NS [-t TEST-NS …] [-d DEF …] [--timeout MS]")

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
        target (some-> (first arguments) symbol)]
    (if (or errors (nil? target) (empty? (:test options)))
      (do (println (str/join "\n" (concat errors [usage summary])))
          (System/exit 2))
      (let [survivors (mutate target (:test options) {:defs (:def options)
                                                      :timeout (:timeout options)})]
        (shutdown-agents)
        (System/exit (if (seq survivors) 1 0))))))
