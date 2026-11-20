(ns cljs-patrol.dev.mutate
  "Mutation testing in one warm JVM: break one operator of a namespace at a time, and check its tests notice.

  Each mutant swaps one symbol or literal, such as `when` for `when-not` or `<` for `<=`, in one
  top-level form. That form is evaluated over the namespace, the test namespaces run until the
  first failure, and the original form is evaluated back. A mutant the tests still pass on is a
  survivor: a branch no test pins. One that cannot be read is unviable, and one whose tests run
  too long timed out; neither counts toward the score."
  (:require
   [cljs-patrol.parser :as parser]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [clojure.test :as t]
   [clojure.tools.cli :as cli]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:gen-class))

(def swaps
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
    (into {true false
           false true}
          (mapcat (fn [[a b]] [[a b] [b a]]))
          pairs)))

(def ^:private skipped-heads #{"ns" "comment"})

(def loaded-once-heads
  "Forms Clojure keeps from the first load, or that other code captures then, so evaluating a mutant of one proves nothing."
  #{"defmacro" "defmulti" "defonce" "defprotocol" "defrecord" "deftype"})

(def ^:private definition-heads
  "Forms whose second form names what they define."
  #{"def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defonce" "defprotocol" "defrecord" "deftype"})

(defn- inside-skipped?
  "True when `loc` sits in metadata, a `#_`, a quoted form or a `comment`, none of which a test reaches."
  [loc]
  (some #(or (contains? #{:uneval :quote :syntax-quote :meta} (z/tag %))
             (and (= :list (z/tag %)) (= "comment" (parser/sym-name (z/down %)))))
        (take-while some? (iterate z/up loc))))

(defn- replacement
  "Return the token value to put in place of the one at `loc`, or nil when no operator swaps it."
  [loc]
  (when (= :token (z/tag loc))
    (let [value (try (z/sexpr loc) (catch Exception _ ::unreadable))]
      (when (contains? swaps value)
        (get swaps value)))))

(defn- form-name
  "Name a top-level form by what it defines, or by its head and line, such as `extend-type@12`."
  [loc head]
  (or (when (contains? definition-heads head)
        (some-> (parser/declared-name-loc loc) parser/sym-name))
      (str head "@" (first (parser/position loc)))))

(defn top-level-forms
  "Return {:loc … :head … :name … :loaded-once? …} for each top-level list of `source` but the ns form and `comment`."
  [source]
  (->> (z/of-string source)
       (iterate z/right)
       (take-while some?)
       (keep (fn [loc]
               (when (= :list (z/tag loc))
                 (let [head (parser/sym-name (z/down loc))]
                   (when-not (contains? skipped-heads head)
                     {:loc loc
                      :head head
                      :name (form-name loc head)
                      :loaded-once? (contains? loaded-once-heads head)})))))))

(defn- form-tokens
  "Return every loc inside the top-level form at `loc`, the form included."
  [loc]
  (let [after (some-> (z/right loc) z/node)]
    (->> loc
         (iterate z/next)
         (take-while #(and (not (z/end? %)) (not (identical? after (z/node %))))))))

(defn mutants
  "Return a mutant for every swap the top-level `forms` allow, those Clojure loads only once left out.

  Each is {:def … :row … :col … :from … :to … :form-loc … :mutant-loc …}; the mutated text
  of its form is built only when the mutant runs."
  [forms]
  (for [{:keys [loc name loaded-once?]} forms
        :when (not loaded-once?)
        token (form-tokens loc)
        :let [to (replacement token)]
        :when (and (some? to) (not (inside-skipped? token)))
        :let [[row col] (parser/position token)]]
    {:def name
     :row row
     :col col
     :from (z/string token)
     :to (pr-str to)
     :form-loc loc
     :mutant-loc token}))

(defn changed-lines
  "Return the line numbers `git diff -U0` output adds or changes in the new version of a file."
  [diff-text]
  (into #{}
        (mapcat (fn [[_ start length]]
                  (let [start (parse-long start)
                        length (if length (parse-long length) 1)]
                    (range start (+ start length)))))
        (re-seq #"(?m)^@@ -\S+ \+(\d+)(?:,(\d+))? @@" diff-text)))

(defn- setup-error [message]
  (ex-info message {::setup true}))

(defn- git [dir & args]
  (apply shell/sh "git" "-C" dir args))

(defn- lines-since
  "Return the lines of `file` that differ from where the current branch left git `ref`.

  The diff is taken against the merge base, so commits made on `ref` since then do not count
  as the branch's changes. A file the merge base lacks is new, and every line of it counts."
  [file ref]
  (let [dir (str (.getParentFile (io/file file)))
        path (str (io/file file))]
    (when-not (zero? (:exit (git dir "rev-parse" "--verify" "--quiet" (str ref "^{commit}"))))
      (throw (setup-error (str "No git commit named " ref "."))))
    (let [base (str/trim (:out (git dir "merge-base" ref "HEAD")))
          tracked (str/trim (:out (git dir "ls-files" "--full-name" path)))
          at-base? (and (seq base)
                        (seq tracked)
                        (zero? (:exit (git dir "cat-file" "-e" (str base ":" tracked)))))]
      (if at-base?
        (let [{:keys [exit out err]} (git dir "diff" "-U0" base "--" path)]
          (when-not (zero? exit)
            (throw (setup-error (str "git diff against " ref " failed: " (str/trim err)))))
          (changed-lines out))
        :all-lines))))

(defn- source-file [ns-sym]
  (let [path (-> (name ns-sym) (str/replace "-" "_") (str/replace "." "/"))]
    (or (some #(some-> (io/resource (str path %)) io/file) [".clj" ".cljc"])
        (throw (setup-error (str "No source for " ns-sym " on the classpath."))))))

(defn load-text
  "Read and evaluate every form of `text` in `target`, reader conditionals read for Clojure."
  [target text]
  (binding [*ns* (the-ns target)]
    (let [reader (java.io.PushbackReader. (java.io.StringReader. text))]
      (loop []
        (let [form (read {:read-cond :allow
                          :eof ::eof}
                         reader)]
          (when-not (= ::eof form)
            (eval form)
            (recur)))))))

(defn- stop-at-failure
  "Report a failure or an error by throwing, which ends the test run: one failure decides a mutant is killed."
  [{:keys [type]}]
  (when (contains? #{:fail :error} type)
    (throw (ex-info "A test failed." {::failed true}))))

(defn- run-tests
  "Run the test namespaces quietly in a thread of their own, and return {:outcome … :thread …}.

  The outcome is :passed, :failed or :timeout. With `stop-early?` the run ends at the first
  failure. A timed-out thread is interrupted, but a loop that never checks for it keeps
  running, so the caller decides whether to go on."
  [test-nss timeout-ms stop-early?]
  (let [result (promise)
        thread (Thread. ^Runnable (fn []
                                    (deliver result
                                             (try
                                               (binding [t/*test-out* (java.io.StringWriter.)
                                                         t/report (if stop-early? stop-at-failure t/report)]
                                                 (let [{:keys [fail error]} (apply t/run-tests test-nss)]
                                                   (if (and (not stop-early?) (pos? (+ fail error))) :failed :passed)))
                                               (catch Throwable _ :failed)))))]
    (.setDaemon thread true)
    (.start thread)
    (.join thread (long timeout-ms))
    (if (realized? result)
      {:outcome @result}
      (do (.interrupt thread)
          {:outcome :timeout
           :thread thread}))))

(def ^:private max-stuck-threads
  "Timed-out test runs that may still be running before later results stop being trustworthy."
  2)

(defn- run-mutant
  "Apply a mutant, run the tests, restore the namespace, and return the mutant with its :outcome.

  A form that cannot be read is unviable. One that reads but throws while it loads, such as a
  `def` whose value now fails, was caught by loading it, and is killed."
  [{:keys [target test-nss timeout-ms]}
   {:keys [form-loc mutant-loc to]
    :as mutant}]
  (let [mutated-text (-> mutant-loc (z/replace (n/token-node (edn/read-string to))) parser/top-level-loc z/string)
        load-outcome (try
                       (load-text target mutated-text)
                       nil
                       (catch clojure.lang.LispReader$ReaderException _ :unviable)
                       (catch Throwable _ :failed))]
    (try
      (if load-outcome
        (assoc mutant :outcome load-outcome)
        (let [{:keys [outcome thread]} (run-tests test-nss timeout-ms true)]
          (assoc mutant
                 :outcome outcome
                 :thread thread)))
      (finally (load-text target (z/string form-loc))))))

(def ^:private baseline-limit-ms 600000)

(defn- baseline-ms
  "Run the tests on the unmutated namespace and return how long they took, or throw when they fail."
  [test-nss timeout-ms]
  (let [limit (or timeout-ms baseline-limit-ms)
        started (System/nanoTime)
        {:keys [outcome]} (run-tests test-nss limit false)]
    (case outcome
      :passed (/ (- (System/nanoTime) started) 1e6)
      :timeout (throw (setup-error (str "The tests ran past " limit " ms before any mutation. Raise --timeout.")))
      (throw (setup-error "The tests fail before any mutation; fix them first.")))))

(defn- outcome-key [outcome]
  (case outcome
    :passed :survived
    :failed :killed
    outcome))

(defn- outcome-counts [results]
  (merge {:killed 0
          :survived 0
          :unviable 0
          :timeout 0}
         (frequencies (map (comp outcome-key :outcome) results))))

(defn summarize
  "Return the report of a run: each definition's counts, the definitions not mutated and why, and every mutant's outcome."
  [{:keys [defs-in-scope loaded-once results stopped]}]
  (let [by-def (group-by :def results)
        counts (outcome-counts results)
        scored (+ (:killed counts) (:survived counts))]
    {:definitions (vec (for [def-name (distinct defs-in-scope)
                             :let [def-results (get by-def def-name)]
                             :when (seq def-results)]
                         (assoc (outcome-counts def-results)
                                :def def-name
                                :mutants (count def-results))))
     :not-mutated (vec (distinct (remove #(contains? by-def %) defs-in-scope)))
     :loaded-once (vec (distinct loaded-once))
     :totals (assoc counts :mutants (count results))
     :score (when (pos? scored) (double (/ (:killed counts) scored)))
     :mutants (mapv #(-> %
                         (select-keys [:def :row :col :from :to])
                         (assoc :outcome (outcome-key (:outcome %))))
                    results)
     :stopped stopped}))

(defn- print-report [{:keys [definitions not-mutated loaded-once totals score mutants stopped]} source-lines since]
  (println)
  (pprint/print-table [:def :mutants :killed :survived :unviable :timeout] definitions)
  (when (seq not-mutated)
    (println (str "\nNot mutated, no operator " (if since "on the changed lines" "applies") ": "
                  (str/join ", " not-mutated))))
  (when (seq loaded-once)
    (println (str "\nNot mutated, loaded once: " (str/join ", " loaded-once))))
  (doseq [[outcome title] [[:survived "Survived"] [:unviable "Unviable, could not be read"] [:timeout "Timed out"]]
          :let [shown (filter #(= outcome (:outcome %)) mutants)]
          :when (seq shown)]
    (println (str "\n" title ":"))
    (doseq [{:keys [def row col from to]} shown]
      (println (format "  %d:%d  %s -> %s  in %s" row col from to def))
      (println (str "      " (str/trim (nth source-lines (dec row) ""))))))
  (println (format "\n%d mutants: %d killed, %d survived, %d unviable, %d timed out. Score %s."
                   (:mutants totals) (:killed totals) (:survived totals) (:unviable totals) (:timeout totals)
                   (if score (format "%.0f%%" (* 100 score)) "n/a")))
  (when stopped
    (println (str "\nStopped early: " stopped))))

(defn- form-lines [{:keys [loc]}]
  (let [[[start] [end]] (parser/position-span loc)]
    (range start (inc end))))

(defn scope
  "Return the definitions and mutants a run covers, and check every definition named with `-d` exists."
  [source {:keys [defs lines]}]
  (let [forms (top-level-forms source)
        wanted (set defs)
        unknown (remove (set (map :name forms)) wanted)]
    (when (seq unknown)
      (throw (setup-error (str "No top-level definition named " (str/join ", " (sort unknown)) "."))))
    (let [in-scope (filter #(and (or (empty? wanted) (contains? wanted (:name %)))
                                 (or (not (set? lines)) (some lines (form-lines %))))
                           forms)]
      {:defs-in-scope (mapv :name (remove :loaded-once? in-scope))
       :loaded-once (mapv :name (filter :loaded-once? in-scope))
       :selected (filter #(or (not (set? lines)) (contains? lines (:row %))) (mutants in-scope))})))

(defn- run-all
  "Run each mutant in turn, stopping when timed-out test runs pile up."
  [context selected]
  (loop [[mutant & more] selected
         results []
         stuck []]
    (if (nil? mutant)
      {:results results}
      (let [result (run-mutant context mutant)
            stuck (filterv #(.isAlive ^Thread %) (cond-> stuck (:thread result) (conj (:thread result))))]
        (print (case (:outcome result) :passed "S" :failed "." :unviable "U" :timeout "T"))
        (flush)
        (if (> (count stuck) max-stuck-threads)
          {:results (conj results result)
           :stopped (str (count stuck) " timed-out test runs are still running, so later results would be slowed"
                         " down. Fix the mutants that loop, or raise --timeout.")}
          (recur more (conj results result) stuck))))))

(defn mutate
  "Mutate `target` against `test-nss`, print the report, write it to `report` when given, and return it."
  [target test-nss {:keys [defs since timeout report]}]
  (let [file (source-file target)
        source (slurp file)
        _ (try
            (require target)
            (run! require test-nss)
            (catch Exception e
              (throw (setup-error (str "Could not load the namespaces: " (ex-message e))))))
        {:keys [defs-in-scope loaded-once selected]} (scope source {:defs defs
                                                                    :lines (when since (lines-since file since))})
        timeout-ms (when (seq selected)
                     (or timeout (long (+ 2000 (* 1.5 (baseline-ms test-nss timeout))))))
        _ (println (format "%d mutants of %s against %s%s%s"
                           (count selected) target (str/join ", " test-nss)
                           (if since (str ", lines changed since " since) "")
                           (if timeout-ms (str ", timeout " timeout-ms " ms") "")))
        {:keys [results stopped]} (run-all {:target target
                                            :test-nss test-nss
                                            :timeout-ms timeout-ms}
                                           selected)
        summary (summarize {:defs-in-scope defs-in-scope
                            :loaded-once loaded-once
                            :results results
                            :stopped stopped})]
    (print-report summary (str/split-lines source) since)
    (when report
      (spit report (with-out-str (pprint/pprint (assoc summary
                                                       :target target
                                                       :tests test-nss
                                                       :since since))))
      (println (str "Report written to " report)))
    summary))

(def ^:private cli-options
  [["-t" "--test NS" "A test namespace to run, given once per namespace"
    :multi true
    :default []
    :update-fn conj
    :parse-fn symbol]
   ["-d" "--def NAME" "Mutate only inside this top-level definition, given once per name"
    :multi true
    :default []
    :update-fn conj]
   ["-s" "--since REF" "Mutate only lines the branch changed since it left this git ref, such as main"]
   [nil "--timeout MS" "Milliseconds one test run may take; by default 1.5 times the first run plus 2 s"
    :parse-fn parse-long
    :validate [pos-int? "Must be a positive whole number of milliseconds"]]
   ["-r" "--report FILE" "Write every mutant and its outcome to this EDN file"]])

(def ^:private usage
  "Usage: clojure -M:test:mutate TARGET-NS -t TEST-NS [-t TEST-NS …] [-d DEF …] [-s REF] [--timeout MS] [-r FILE]")

(defn -main
  "Exit 0 when no mutant survives, 1 when one does, and 2 when the run cannot start or stops early."
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
        target (some-> (first arguments) symbol)]
    (if (or errors (nil? target) (empty? (:test options)))
      (do (println (str/join "\n" (concat errors [usage summary])))
          (System/exit 2))
      (try
        (let [{:keys [totals stopped]} (mutate target (:test options) {:defs (:def options)
                                                                       :since (:since options)
                                                                       :timeout (:timeout options)
                                                                       :report (:report options)})]
          (System/exit (cond
                         stopped 2
                         (pos? (:survived totals)) 1
                         :else 0)))
        (catch Throwable e
          (println (if (::setup (ex-data e)) (ex-message e) (str "The run failed: " e)))
          (System/exit 2))))))
