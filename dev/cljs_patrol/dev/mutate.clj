(ns cljs-patrol.dev.mutate
  "Mutation testing in one warm JVM: break one operator of a namespace at a time, and check its tests notice.

  The code under test changes, never the tests. Each mutant swaps one symbol or literal, such as
  `when` for `when-not` or `<` for `<=`, in one definition. That form is evaluated over the
  namespace, the test namespaces run until the first failure, and the original form is evaluated
  back. A mutant the tests still pass on is a survivor: a branch no test pins. One that cannot be
  read or compiled is unviable, and one whose tests run too long timed out; neither is scored."
  (:require
   [cljs-patrol.parser :as parser]
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

(def ^:private mutated-heads
  "Definitions that evaluating again replaces, so a mutant of one takes effect."
  #{"def" "defn" "defn-" "defmethod"})

(def loaded-once-heads
  "Definitions Clojure keeps from the first load, or that other code captures then.

  Evaluating a mutant of one proves nothing, so they are not mutated."
  #{"defmacro" "defmulti" "defonce" "defprotocol" "defrecord" "deftype"})

(defn- read-branch
  "Return the form a `#?` or `#?@` reader conditional at `loc` gives Clojure: the first `:clj` or `:default` branch."
  [loc]
  (when (and (= :reader-macro (z/tag loc)) (contains? #{"?" "?@"} (some-> loc z/down z/string)))
    (some->> (some-> loc z/down z/right)
             parser/written-child-locs
             (partition 2)
             (some (fn [[feature form]] (when (contains? #{:clj :default} (z/sexpr feature)) form))))))

(defn- unread-branch?
  "True when `loc` is a branch of a reader conditional that Clojure does not read, or one of its feature keywords."
  [loc]
  (let [conditional (some-> loc z/up z/up)]
    (and (= :reader-macro (some-> conditional z/tag))
         (not (identical? (z/node loc) (some-> (read-branch conditional) z/node))))))

(defn- attr-map?
  "True when `loc` is the attribute map of a `defn`, written after its name and before its parameter vector."
  [loc]
  (let [parent (z/up loc)]
    (and (= :map (z/tag loc))
         (= :list (some-> parent z/tag))
         (contains? #{"defn" "defn-"} (parser/sym-name (z/down parent)))
         (not-any? #(= :vector (z/tag %)) (take-while some? (iterate z/left loc))))))

(defn- skipped?
  "True when `loc` holds no code a test reaches through this namespace.

  That is metadata, a `defn`'s attribute map, a `#_`, a quoted form, a `comment`, and a
  reader-conditional branch Clojure does not read."
  [loc]
  (some (fn [ancestor]
          (let [parent (z/up ancestor)]
            (or (contains? #{:uneval :quote :syntax-quote} (z/tag ancestor))
                (and (= :list (z/tag ancestor)) (= "comment" (parser/sym-name (z/down ancestor))))
                (and (= :meta (some-> parent z/tag)) (identical? (z/node ancestor) (z/node (z/down parent))))
                (attr-map? ancestor)
                (unread-branch? ancestor))))
        (take-while some? (iterate z/up loc))))

(defn- replacement
  "Return the value to put in place of the token at `loc`, or nil when no operator swaps it."
  [loc]
  (when (= :token (z/tag loc))
    (get swaps (try (z/sexpr loc) (catch Exception _ nil)))))

(defn- form-name
  "Name a top-level form by what it defines, or by its head and line, such as `extend-type@12`."
  [loc head]
  (or (when (or (contains? mutated-heads head) (contains? loaded-once-heads head))
        (some-> (parser/declared-name-loc loc) parser/sym-name))
      (str head "@" (first (parser/position loc)))))

(defn- unwrapped
  "Return the forms a top-level form at `loc` stands for, each with the top-level form it was written in.

  Metadata in front of a form is peeled off, and a reader conditional gives the branch Clojure
  reads, a splicing one each form of it."
  [loc]
  (case (z/tag loc)
    :meta (map #(assoc % :wrapper loc) (unwrapped (z/rightmost (z/down loc))))
    :reader-macro (when-let [branch (read-branch loc)]
                    (if (and (= "?@" (z/string (z/down loc))) (= :vector (z/tag branch)))
                      (mapcat unwrapped (parser/written-child-locs branch))
                      (unwrapped branch)))
    [{:loc loc}]))

(defn top-level-forms
  "Return {:loc … :wrapper … :head … :name … :kind …} for each top-level form of `source` but `ns` and `comment`.

  `:kind` is :mutated for a definition evaluating again replaces, :loaded-once for one it does
  not, and :other for any other form, whose effects would run again with every mutant. A form
  under metadata keeps that metadata as its `:wrapper`."
  [source]
  (->> (z/of-string source)
       (iterate z/right)
       (take-while some?)
       (mapcat unwrapped)
       (keep (fn [{:keys [loc]
                   :as form}]
               (when (= :list (z/tag loc))
                 (let [head (parser/sym-name (z/down loc))]
                   (when-not (contains? #{"ns" "comment"} head)
                     (assoc form
                            :head head
                            :name (form-name loc head)
                            :kind (cond
                                    (contains? mutated-heads head) :mutated
                                    (contains? loaded-once-heads head) :loaded-once
                                    :else :other)))))))))

(defn- form-locs
  "Return every loc inside the form at `loc`, the form included, in a zipper of that form alone."
  [loc]
  (->> (z/subzip loc) (iterate z/next) (take-while (complement z/end?))))

(defn mutants
  "Return a mutant for every swap the definitions among the top-level `forms` allow.

  Each is {:def … :row … :col … :from … :to … :form-loc … :mutant-loc …}; the mutated text of
  its form is built only when the mutant runs."
  [forms]
  (for [{:keys [loc wrapper name kind]} forms
        :when (= :mutated kind)
        token (form-locs loc)
        :let [to (replacement token)]
        :when (and (some? to) (not (skipped? token)))
        :let [[row col] (parser/position token)]]
    {:def name
     :row row
     :col col
     :from (z/string token)
     :to to
     :form-loc loc
     :wrapper wrapper
     :mutant-loc token}))

(defn changed-lines
  "Return the line numbers `git diff -U0` output adds or changes in the new version of a file.

  A hunk that only deletes marks the line it deleted after, so the definition that lost the
  lines still counts as changed."
  [diff-text]
  (into #{}
        (mapcat (fn [[_ start length]]
                  (let [start (parse-long start)
                        length (if length (parse-long length) 1)]
                    (if (zero? length)
                      [(max 1 start)]
                      (range start (+ start length))))))
        (re-seq #"(?m)^@@ -\S+ \+(\d+)(?:,(\d+))? @@" diff-text)))

(defn- setup-error [message]
  (ex-info message {::setup true}))

(defn- git [dir & args]
  (apply shell/sh "git" "-C" dir args))

(defn lines-since
  "Return the lines of `file` that differ from where the current branch left git `ref`.

  The diff is taken against the merge base, so commits made on `ref` since then do not count
  as the branch's changes. A file the merge base lacks is new, and every line of it counts."
  [file ref]
  (let [dir (str (.getParentFile (io/file file)))
        path (str (io/file file))]
    (when-not (= "true" (str/trim (:out (git dir "rev-parse" "--is-inside-work-tree"))))
      (throw (setup-error (str file " is not in a git work tree, so -s cannot compare it."))))
    (when-not (zero? (:exit (git dir "rev-parse" "--verify" "--quiet" (str ref "^{commit}"))))
      (throw (setup-error (str "No git commit named " ref "."))))
    (let [{base-exit :exit
           base-out :out
           base-err :err} (git dir "merge-base" ref "HEAD")
          base (str/trim base-out)]
      (when-not (zero? base-exit)
        (throw (setup-error (str "No merge base of " ref " and HEAD; fetch more history. " (str/trim base-err)))))
      (let [tracked (str/trim (:out (git dir "ls-files" "--full-name" path)))
            at-base? (and (seq tracked) (zero? (:exit (git dir "cat-file" "-e" (str base ":" tracked)))))]
        (if at-base?
          (let [{:keys [exit out err]} (git dir "diff" "--no-color" "--no-ext-diff" "-U0" base "--" path)]
            (when-not (zero? exit)
              (throw (setup-error (str "git diff against " ref " failed: " (str/trim err)))))
            (changed-lines out))
          :all-lines)))))

(defn- source-file [ns-sym]
  (let [path (-> (name ns-sym) (str/replace "-" "_") (str/replace "." "/"))]
    (or (some #(some-> (io/resource (str path %)) io/file) [".clj" ".cljc"])
        (throw (setup-error (str "No source for " ns-sym " on the classpath."))))))

(defn load-text
  "Read and evaluate every form of `text` in `target`, reader conditionals read for Clojure.

  Line numbers start at `line`, so the vars keep the position of their source."
  ([target text] (load-text target text {}))
  ([target text {:keys [file line]}]
   (binding [*ns* (the-ns target)
             *file* (or file *file*)]
     (let [reader (doto (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. text))
                    (.setLineNumber (int (or line 1))))]
       (loop []
         (let [form (read {:read-cond :allow
                           :eof ::eof}
                          reader)]
           (when-not (= ::eof form)
             (eval form)
             (recur))))))))

(def ^:private unviable-phases
  "Phases of a Clojure error that come before the code runs: the mutant could not be read or compiled."
  #{:read-source :macro-syntax-check :macroexpansion :compile-syntax-check :compilation})

(defn load-outcome
  "Return nil when loading succeeds, :unviable for a read or compile error, and :failed for one the code threw."
  [load!]
  (try
    (load!)
    nil
    (catch Throwable e
      (if (or (instance? clojure.lang.LispReader$ReaderException e)
              (contains? unviable-phases (:clojure.error/phase (ex-data e))))
        :unviable
        :failed))))

(defn run-tests
  "Run the test namespaces quietly in a thread of their own, and return {:outcome … :output … :thread …}.

  The outcome is :passed, :failed or :timeout. A failure is recorded before the run is cut
  short, so an assertion that catches exceptions, such as `thrown?`, cannot hide it, and a
  run that failed before it timed out counts as failed. A timed-out thread is interrupted, but
  a loop that never checks for it keeps running."
  [test-nss timeout-ms]
  (let [failed (atom false)
        output (java.io.StringWriter.)
        default-report (.getRawRoot #'t/report)
        report (fn [{:keys [type]
                     :as event}]
                 (default-report event)
                 (when (contains? #{:fail :error} type)
                   (reset! failed true)
                   (throw (AssertionError. "A test failed; the mutant is killed."))))
        result (promise)
        thread (Thread. ^Runnable (fn []
                                    (deliver result
                                             (try
                                               (binding [t/*test-out* output
                                                         *out* output
                                                         t/report report]
                                                 (apply t/run-tests test-nss))
                                               (if @failed :failed :passed)
                                               (catch Throwable _ :failed)))))]
    (.setDaemon thread true)
    (.start thread)
    (.join thread (long timeout-ms))
    (if (realized? result)
      {:outcome @result
       :output (str output)}
      (do (.interrupt thread)
          {:outcome (if @failed :failed :timeout)
           :thread thread}))))

(def ^:private stuck-grace-ms
  "How long an interrupted test thread may take to end before the run counts it as stuck."
  500)

(defn- run-mutant
  "Apply a mutant, run the tests, restore the namespace, and return the mutant with its :outcome.

  A form that cannot be read or compiled is unviable. One that throws while it loads, such as
  a `def` whose value now fails, was caught by loading it, and is killed. A restore that fails
  is returned as :restore-error, since the namespace is then left mutated."
  [{:keys [target file test-nss timeout-ms]}
   {:keys [form-loc wrapper mutant-loc to]
    :as mutant}]
  (let [line (first (parser/position (or wrapper form-loc)))
        original (z/string form-loc)
        in-wrapper (fn [form-text]
                     (if wrapper
                       (let [wrapper-text (z/string wrapper)
                             start (str/last-index-of wrapper-text original)]
                         (str (subs wrapper-text 0 start) form-text (subs wrapper-text (+ start (count original)))))
                       form-text))
        mutated-text (in-wrapper (-> mutant-loc (z/replace (n/token-node to)) z/root n/string))
        load-result (load-outcome #(load-text target mutated-text {:file file
                                                                   :line line}))
        result (if load-result
                 (assoc mutant :outcome load-result)
                 (let [{:keys [outcome thread]} (run-tests test-nss timeout-ms)]
                   (cond-> (assoc mutant :outcome outcome)
                     thread (assoc :stuck? (do (.join ^Thread thread (long stuck-grace-ms))
                                               (.isAlive ^Thread thread))))))]
    (try
      (load-text target (in-wrapper original) {:file file
                                               :line line})
      result
      (catch Throwable e
        (assoc result :restore-error (ex-message e))))))

(def ^:private baseline-limit-ms 600000)

(defn- baseline-ms
  "Run the tests on the unmutated namespace and return how long they took, or throw when they fail."
  [test-nss timeout-ms]
  (let [limit (or timeout-ms baseline-limit-ms)
        started (System/nanoTime)
        {:keys [outcome output]} (run-tests test-nss limit)]
    (case outcome
      :passed (/ (- (System/nanoTime) started) 1e6)
      :timeout (throw (setup-error (str "The tests ran past " limit " ms before any mutation. Raise --timeout.")))
      (throw (setup-error (str "The tests fail before any mutation; fix them first.\n" output))))))

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
  [{:keys [defs-in-scope loaded-once other-forms unreached results stopped]}]
  (let [by-def (group-by :def results)
        counts (outcome-counts results)
        scored (+ (:killed counts) (:survived counts))
        unreached-counts (frequencies (map :def unreached))]
    {:definitions (vec (for [def-name (distinct defs-in-scope)
                             :let [def-results (get by-def def-name)]
                             :when (seq def-results)]
                         (assoc (outcome-counts def-results)
                                :def def-name
                                :mutants (count def-results))))
     :not-mutated (->> defs-in-scope
                       (remove #(or (contains? by-def %) (contains? unreached-counts %)))
                       distinct
                       vec)
     :loaded-once (vec (distinct loaded-once))
     :other-forms (vec (distinct other-forms))
     :unreached (vec (for [def-name (distinct (map :def unreached))]
                       {:def def-name
                        :mutants (get unreached-counts def-name)}))
     :totals (assoc counts :mutants (count results))
     :score (when (pos? scored) (double (/ (:killed counts) scored)))
     :mutants (mapv #(-> %
                         (select-keys [:def :row :col :from])
                         (assoc :to (pr-str (:to %))
                                :outcome (outcome-key (:outcome %))))
                    results)
     :stopped stopped}))

(defn- print-list [title names]
  (when (seq names)
    (println (str "\n" title ": " (str/join ", " names)))))

(defn- print-report [{:keys [definitions not-mutated loaded-once other-forms unreached totals score mutants stopped]}
                     source-lines
                     since]
  (println)
  (pprint/print-table [:def :mutants :killed :survived :unviable :timeout] definitions)
  (print-list (str "Not mutated, no operator " (if since "on the changed lines" "applies")) not-mutated)
  (print-list "Not mutated, loaded once" loaded-once)
  (print-list "Not mutated, not a definition" other-forms)
  (print-list "Not reached, the run stopped early" (map #(str (:def %) " (" (:mutants %) " mutants)") unreached))
  (doseq [[outcome title] [[:survived "Survived"] [:unviable "Unviable, could not be read or compiled"]
                           [:timeout "Timed out"]]
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
                           forms)
          of-kind (fn [kind] (filter #(= kind (:kind %)) in-scope))]
      {:defs-in-scope (mapv :name (of-kind :mutated))
       :loaded-once (mapv #(str (:name %) " (" (:head %) ")") (of-kind :loaded-once))
       :other-forms (mapv :name (of-kind :other))
       :selected (filter #(or (not (set? lines)) (contains? lines (:row %))) (mutants in-scope))})))

(defn- run-all
  "Run each mutant in turn, stopping at the first test run that keeps looping or the first restore that fails."
  [context selected]
  (loop [[mutant & more] selected
         results []]
    (if (nil? mutant)
      {:results results}
      (let [{:keys [stuck? restore-error]
             :as result} (run-mutant context mutant)
            results (conj results result)]
        (print (case (:outcome result) :passed "S" :failed "." :unviable "U" :timeout "T"))
        (flush)
        (cond
          stuck? {:results results
                  :unreached more
                  :stopped (str "the tests of the mutant at " (:row result) ":" (:col result) " still run after the"
                                " timeout, and would disturb every later run. Fix the loop, or raise --timeout.")}
          restore-error {:results results
                         :unreached more
                         :stopped (str "restoring " (:def result) " failed, so the namespace stays mutated: "
                                       restore-error)}
          :else (recur more results))))))

(defn mutate-source
  "Mutate `target`, whose `source` is loaded from `file`, against `test-nss`, print the report, and return it.

  The tests must pass before any mutation, whether or not `timeout` is given. The report is
  also written to `report` when given."
  [target file source test-nss {:keys [defs since timeout report]}]
  (let [{:keys [defs-in-scope loaded-once other-forms selected]} (scope source {:defs defs
                                                                                :lines (when since
                                                                                         (lines-since file since))})
        baseline (when (seq selected) (baseline-ms test-nss timeout))
        timeout-ms (when baseline (or timeout (long (+ 2000 (* 1.5 baseline)))))
        _ (println (format "%d mutants of %s against %s%s%s"
                           (count selected) target (str/join ", " test-nss)
                           (if since (str ", lines changed since " since) "")
                           (if timeout-ms (str ", timeout " timeout-ms " ms") "")))
        {:keys [results unreached stopped]} (run-all {:target target
                                                      :file (str file)
                                                      :test-nss test-nss
                                                      :timeout-ms timeout-ms}
                                                     selected)
        summary (summarize {:defs-in-scope defs-in-scope
                            :loaded-once loaded-once
                            :other-forms other-forms
                            :unreached unreached
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

(defn mutate
  "Mutate `target`, found on the classpath, against `test-nss`, as [[mutate-source]] does."
  [target test-nss options]
  (let [file (source-file target)]
    (try
      (require target)
      (run! require test-nss)
      (catch Exception e
        (throw (setup-error (str "Could not load the namespaces: " (ex-message e))))))
    (mutate-source target file (slurp file) test-nss options)))

(defn exit-code
  "Return 2 when the run stopped early, 1 when a mutant survived, and 0 otherwise."
  [{:keys [totals stopped]}]
  (cond
    stopped 2
    (pos? (:survived totals)) 1
    :else 0))

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
        (System/exit (exit-code (mutate target (:test options) {:defs (:def options)
                                                                :since (:since options)
                                                                :timeout (:timeout options)
                                                                :report (:report options)})))
        (catch Throwable e
          (println (if (::setup (ex-data e)) (ex-message e) (str "The run failed: " e)))
          (System/exit 2))))))
