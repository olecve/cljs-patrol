(ns cljs-patrol.dev.mutate
  "Mutation testing in one warm JVM: break one operator of a namespace at a time, and check its tests notice.

  The code under test changes, never the tests. A first run records which tests call each
  function. Each mutant swaps one symbol or literal, such as `when` for `when-not` or `<` for
  `<=`, in one definition. That form is evaluated over the namespace, the tests that call it run
  until the first failure, and the original form is evaluated back. A mutant the tests still pass
  on is a survivor: a branch no test pins. A function no test calls has no coverage. A mutant
  that cannot be read or compiled is unviable, and one whose tests run too long timed out;
  neither is scored."
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

(defn- parameter-vector?
  "True when `loc` is a vector, also one under a type hint such as `^String [x]`."
  [loc]
  (or (= :vector (z/tag loc))
      (and (= :meta (z/tag loc)) (= :vector (some-> loc z/down z/rightmost z/tag)))))

(defn- attr-map?
  "True when `loc` is the attribute map of a `defn`: before its parameter vector, or after its last arity."
  [loc]
  (let [parent (z/up loc)]
    (and (= :map (z/tag loc))
         (= :list (some-> parent z/tag))
         (contains? #{"defn" "defn-"} (parser/sym-name (z/down parent)))
         (not-any? parameter-vector? (take-while some? (iterate z/left loc))))))

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

(defn- skip-reason
  "Return {:reason …} for `:mutate/skip` on the name or in the attribute map of the definition at `loc`, as written.

  Return nil when neither has the key."
  [loc]
  (let [name-slot (some-> loc z/down z/right)
        attr-map (->> (some-> name-slot z/right)
                      (iterate z/right)
                      (take-while some?)
                      (filter attr-map?)
                      first)]
    (some (fn [metadata]
            (when (contains? metadata :mutate/skip)
              {:reason (:mutate/skip metadata)}))
          [(when (= :meta (some-> name-slot z/tag)) (meta (z/sexpr name-slot)))
           (some-> attr-map z/sexpr)])))

(defn mutants
  "Return a mutant for every swap the definitions among the top-level `forms` allow.

  Each is {:def … :row … :col … :from … :to … :form-loc … :mutant-loc …}; the mutated text of
  its form is built only when the mutant runs. A definition with `:mutate/skip` on its name or in
  its attribute map gives each of its mutants that reason as :skip-reason."
  [forms]
  (for [{:keys [loc wrapper name kind]} forms
        :when (= :mutated kind)
        :let [reason (:reason (skip-reason loc))]
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
     :mutant-loc token
     :skip-reason reason}))

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

(def ^:private skip-marker
  "A `; mutate: skip OPERATOR: REASON` comment."
  #"^;+\s*mutate: skip\s+(\S+):\s+(\S.*?)\s*$")

(def ^:private skip-marker-like
  "A comment that starts like a skip marker, written right or not."
  #"(?i)^;+\s*mutate\W*skip")

(defn- code-loc?
  "True when `loc` is code that runs: not whitespace, a newline, a comma, a comment or a `#_` form."
  [loc]
  (not (contains? #{:whitespace :newline :comma :comment :uneval} (z/tag loc))))

(defn skip-markers
  "Return each `; mutate: skip OPERATOR: REASON` comment of `source` as {:line … :row … :operator … :reason …}.

  `:line` is the comment's own line, and `:row` the line it skips: its own line when code comes
  before it there, else the line of the next code in the same form, or nil when none follows.
  Only comments count, so the words in a string or docstring are no marker. A comment that
  starts like a marker but does not read as one stops the run."
  [source]
  (let [lines (str/split-lines source)
        root (let [loc (z/of-string source)] (or (z/up loc) loc))]
    (vec (for [loc (->> root
                        (iterate z/next*)
                        (take-while (complement z/end?)))
               :when (= :comment (z/tag loc))
               :let [text (str/trim (z/string loc))]
               :when (re-find skip-marker-like text)
               :let [[line col] (parser/position loc)
                     [_ operator reason] (re-find skip-marker text)]]
           (if-not reason
             (throw (setup-error (str "The comment on line " line " reads like a skip marker but is not one."
                                      " Write it as ; mutate: skip OPERATOR: REASON, such as"
                                      " ; mutate: skip first: the list holds one item.")))
             {:line line
              :row (if (str/blank? (subs (nth lines (dec line)) 0 (dec col)))
                     (some->> (iterate z/right* loc)
                              rest
                              (take-while some?)
                              (filter code-loc?)
                              first
                              parser/position
                              first)
                     line)
              :operator operator
              :reason reason})))))

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

(defn relative-path
  "Return the path of `file` from the working directory, or the whole path when it lies outside it."
  [file]
  (let [path (.toPath (.getCanonicalFile (io/file file)))
        cwd (.toPath (.getCanonicalFile (io/file ".")))]
    (str (if (.startsWith path cwd) (.relativize cwd path) path))))

(defn apply-mutant
  "Write the mutant at `position`, written ROW:COL, into `file`, whose text is `source`, and return how to restore it.

  The file must be in a git work tree with no uncommitted change, so that `git restore` brings
  back exactly what was there. Only the one token changes."
  [file source position]
  (let [[_ row col] (re-find #"^(\d+):(\d+)$" (str position))
        row (some-> row parse-long)
        col (some-> col parse-long)
        _ (when-not row
            (throw (setup-error (str "--apply takes ROW:COL, such as 490:43, not " position "."))))
        on-row (->> source top-level-forms mutants (filter #(= row (:row %))))
        {:keys [from to]
         function :def
         :as mutant} (first (filter #(= col (:col %)) on-row)) ; mutate: skip first: one mutant per line and column
        dir (str (.getParentFile (io/file file)))
        path (relative-path file)]
    (when-not (= "true" (str/trim (:out (git dir "rev-parse" "--is-inside-work-tree"))))
      (throw (setup-error (str file " is not in a git work tree, so --apply could not be undone with git restore."))))
    (when (seq (str/trim (:out (git dir "status" "--porcelain" "--" (.getCanonicalPath (io/file file))))))
      (throw (setup-error (str file " has uncommitted changes. Commit or stash them first, so that git restore"
                               " brings back exactly your code."))))
    (when-not mutant
      (throw (setup-error (str "No mutant at " position "."
                               (when (seq on-row)
                                 (str " Line " row " has " (str/join ", " (map #(str row ":" (:col %) " " (:from %)) on-row))
                                      "."))))))
    (let [lines (str/split source #"\n" -1)
          line (nth lines (dec row))
          start (dec col)]
      (spit file (str/join "\n" (assoc lines
                                       (dec row)
                                       (str (subs line 0 start) (pr-str to) (subs line (+ start (count from))))))))
    (str "Applied " position "  " from " -> " (pr-str to) "  in " function " to " path ".\n"
         "Run the tests and watch the new test fail, then restore the file with:\n"
         "  git restore " path)))

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
  "Run the test namespaces quietly in a thread of their own, and return {:outcome … :killed-by … :output … :thread …}.

  Given `test-vars`, only those tests run, with their namespaces' fixtures. The outcome is
  :passed, :failed or :timeout, and :killed-by is the test that failed first. A failure is
  recorded before the run is cut short, so an assertion that catches exceptions, such as
  `thrown?`, cannot hide it, and a run that failed before it timed out counts as failed. A
  timed-out thread is interrupted, but a loop that never checks for it keeps running."
  ([test-nss timeout-ms] (run-tests test-nss timeout-ms nil))
  ([test-nss timeout-ms test-vars]
   (let [failed (atom false)
         killed-by (atom nil)
         output (java.io.StringWriter.)
         default-report (.getRawRoot #'t/report)
         report (fn [{:keys [type]
                      :as event}]
                  (default-report event)
                  (when (contains? #{:fail :error} type)
                    (compare-and-set! killed-by nil (first t/*testing-vars*))
                    (reset! failed true)
                    (throw (AssertionError. "A test failed; the mutant is killed."))))
         result (promise)
         thread (Thread. ^Runnable (fn []
                                     (deliver result
                                              (try
                                                (binding [t/*test-out* output
                                                          *out* output
                                                          t/report report]
                                                  (if (seq test-vars)
                                                    (t/test-vars test-vars)
                                                    (apply t/run-tests test-nss)))
                                                (if @failed :failed :passed)
                                                (catch Throwable _ :failed)))))]
     (.setDaemon thread true)
     (.start thread)
     (.join thread (long timeout-ms))
     (if (realized? result)
       {:outcome @result
        :killed-by @killed-by
        :output (str output)}
       (do (.interrupt thread)
           {:outcome (if @failed :failed :timeout)
            :killed-by @killed-by
            :thread thread})))))

(defn wrappable?
  "True when the var holds a plain function whose calls a wrapper can record.

  A multimethod is no plain function, and a function with a primitive type hint is called
  through an interface a wrapper does not have."
  [v]
  (let [value @v]
    (and (fn? value)
         (not (some #(some (comp #{'long 'double} :tag meta) (cons % %)) (:arglists (meta v)))))))

(defn- test-vars-of [test-nss]
  (for [test-ns test-nss
        v (vals (ns-interns test-ns))
        :when (:test (meta v))]
    v))

(defn coverage
  "Run the tests once with each function of `def-names` wrapped, and record which tests call which function.

  Return the run with :tests {name #{test-var}}, :recorded, the names whose calls could be
  recorded, :reached-outside, the names called while no test ran, such as from a fixture, and
  :hooked?, true when a test namespace runs its tests through `test-ns-hook`. The running test
  is kept where every thread sees it, since the code under test may run on worker threads, and
  the tests run one at a time. A call is recorded under every test that is running, so a test
  that calls another one, inside its own `binding` or `with-redefs`, is selected too. A call
  made without the var, such as through a value the namespace stored when it loaded, is not
  seen, and a call that a background thread makes after its test ended goes to the test that
  runs then."
  [target def-names test-nss timeout-ms]
  (let [calls (atom {})
        reached-outside (atom #{})
        running-tests (atom [])
        vars (->> def-names
                  distinct
                  (keep (fn [def-name]
                          (when-let [v (ns-resolve target (symbol def-name))]
                            (when (wrappable? v) [def-name v @v]))))
                  vec)
        tests (mapv (fn [v] [v (:test (meta v))]) (test-vars-of test-nss))
        record (fn [def-name]
                 (let [running @running-tests]
                   (if (seq running)
                     (when-not (every? (get @calls def-name #{}) running)
                       (swap! calls update def-name (fnil into #{}) running))
                     (when-not (contains? @reached-outside def-name)
                       (swap! reached-outside conj def-name)))))]
    (try
      (doseq [[test-var test-fn] tests]
        (alter-meta! test-var assoc :test (fn []
                                            (swap! running-tests conj test-var)
                                            (try (test-fn) (finally (swap! running-tests pop))))))
      (doseq [[def-name v original] vars]
        (alter-var-root v (constantly (fn [& args]
                                        (record def-name)
                                        (apply original args)))))
      (assoc (run-tests test-nss timeout-ms)
             :tests @calls
             :recorded (set (map first vars))
             :reached-outside @reached-outside
             :hooked? (boolean (some #(contains? (ns-interns %) 'test-ns-hook) test-nss))
             :test-count (count tests))
      (finally
        (doseq [[_ v original] vars]
          (alter-var-root v (constantly original)))
        (doseq [[test-var test-fn] tests]
          (alter-meta! test-var assoc :test test-fn))))))

(defn- default-timeout-ms
  "Return the timeout for a run that took `ms` without a mutation: 1.5 times as long, plus 2 s."
  [ms]
  (long (+ 2000 (* 1.5 ms))))

(defn- timed
  "Call `run!`, which returns a test run, and add how many milliseconds it took as :ms."
  [run!]
  (let [started (System/nanoTime)
        run (run!)]
    (assoc run :ms (/ (- (System/nanoTime) started) 1e6))))

(def ^:private stuck-grace-ms
  "How long an interrupted test thread may take to end before the run counts it as stuck."
  500)

(defn- stuck?
  "True when a timed-out test thread still runs after a short grace, and would disturb every later run."
  [^Thread thread]
  (boolean (when thread
             (.join thread (long stuck-grace-ms))
             (.isAlive thread))))

(defn test-plan
  "Decide which tests the mutants of `def-name` run, and their timeout, before any of them is loaded.

  Return {:kind :subset :tests … :timeout-ms …} for the tests that called it, {:kind
  :uncovered} when the coverage pass saw no call of it, in a test or outside one, {:kind
  :every} when every test must run, and {:kind :stuck} when the tests that call it keep running
  past the timeout without any mutation. Every test runs when selection is off, when the
  definition could not be recorded, when something called it outside a test, when a test
  namespace orders its tests with `test-ns-hook`, when every test calls it anyway, and when its
  tests fail on their own, since one may rely on what an earlier test left behind. That last
  check runs once for each definition, on the unmutated namespace, and its time sets the
  subset's timeout unless one was given."
  [{:keys [test-nss timeout-ms fixed-timeout? select? covered plans]} def-name]
  (let [{:keys [tests recorded reached-outside hooked? test-count]} covered
        def-tests (get tests def-name)
        decide (fn []
                 (cond
                   (or (not (contains? recorded def-name)) (contains? reached-outside def-name)) {:kind :every}
                   (empty? def-tests) {:kind :uncovered}
                   (or hooked? (not select?) (= test-count (count def-tests))) {:kind :every}
                   :else (let [{:keys [outcome thread ms]} (timed #(run-tests test-nss timeout-ms def-tests))]
                           (cond
                             (stuck? thread) {:kind :stuck}
                             (= :passed outcome) {:kind :subset
                                                  :tests def-tests
                                                  :timeout-ms (if fixed-timeout? timeout-ms (default-timeout-ms ms))}
                             :else {:kind :every}))))]
    (or (get @plans def-name)
        (get (swap! plans assoc def-name (decide)) def-name))))

(defn- run-mutant
  "Apply a mutant, run the tests, restore the namespace, and return the mutant with its :outcome.

  The mutant runs the tests that called its definition in the coverage pass, when that is safe.
  A definition no test called runs every test, and a mutant of it that they all pass has no
  coverage, so a call the pass could not see cannot hide a test. A form that cannot be read or
  compiled is unviable. One that throws while it loads, such as a `def` whose value now fails,
  was caught by loading it, and is killed. A restore that fails is returned as :restore-error,
  since the namespace is then left mutated. A definition whose own tests keep running without a
  mutation is returned as :plan-stuck?, with no outcome, since the mutant never ran."
  [{:keys [target file test-nss timeout-ms]
    :as context}
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
        {:keys [kind tests]
         :as plan} (test-plan context (:def mutant))]
    (if (= :stuck kind)
      (assoc mutant :plan-stuck? true)
      (let [load-result (load-outcome #(load-text target mutated-text {:file file
                                                                       :line line}))
            result (if load-result
                     (assoc mutant
                            :outcome load-result
                            :at-load? true)
                     (let [{:keys [outcome thread killed-by]} (run-tests test-nss (:timeout-ms plan timeout-ms) tests)]
                       (assoc mutant
                              :outcome (if (and (= :passed outcome) (= :uncovered kind)) :no-coverage outcome)
                              :killed-by killed-by
                              :stuck? (stuck? thread))))]
        (try
          (load-text target (in-wrapper original) {:file file
                                                   :line line})
          result
          (catch Throwable e
            (assoc result :restore-error (ex-message e))))))))

(def ^:private baseline-limit-ms 600000)

(defn- baseline
  "Run the tests once on the unmutated namespace, recording which tests call each definition.

  Return {:ms … :covered … :note …}, or throw when the tests fail or run past the limit. When
  only the recording makes the tests fail, such as a test that compares a function with the one
  a value stored, or makes them run past a timeout that was given, the tests run again without
  it. The run then goes on without test selection, times its mutants by that plain run, and
  the note says so. A recorded run whose thread still runs after its timeout stops everything,
  since the plain run would share its state."
  [target def-names test-nss timeout-ms]
  (let [limit (or timeout-ms baseline-limit-ms)
        too-slow #(setup-error (str "The tests ran past " limit " ms before any mutation. Raise --timeout."))
        {:keys [outcome thread]
         :as recorded-run} (timed #(coverage target def-names test-nss limit))]
    (cond
      (= :passed outcome) {:ms (:ms recorded-run)
                           :covered (select-keys recorded-run [:tests :recorded :reached-outside :hooked? :test-count])}
      (or (stuck? thread) (and (= :timeout outcome) (not timeout-ms))) (throw (too-slow))
      :else (let [plain-run (timed #(run-tests test-nss limit))]
              (case (:outcome plain-run)
                :passed {:ms (:ms plain-run)
                         :covered {:recorded #{}}
                         :note (str "Recording which tests call each function made the tests fail or run too long, so"
                                    " every mutant runs every test and none is reported as having no coverage.")}
                :timeout (throw (too-slow))
                (throw (setup-error (str "The tests fail before any mutation; fix them first.\n"
                                         (:output plain-run)))))))))

(defn test-name
  "Name a test var as the reports do: namespace/name."
  [test-var]
  (str (symbol test-var)))

(defn- outcome-key [outcome]
  (case outcome
    :passed :survived
    :failed :killed
    outcome))

(defn- outcome-counts [results]
  (merge {:killed 0
          :survived 0
          :no-coverage 0
          :unviable 0
          :timeout 0}
         (frequencies (map (comp outcome-key :outcome) results))))

(defn summarize
  "Return the report of a run: each definition's counts, the definitions not mutated or with no coverage, and every outcome.

  A definition one mutant of which a test killed or timed out is reached by the tests, so its
  other passing mutants survived, even when the coverage pass saw no call. A mutant that threw
  while it loaded shows nothing about the tests. The score is killed over killed,
  survived and no-coverage mutants; the covered score leaves the mutants no test reaches out, so it
  shows how well the tests check the code they run. Each definition names the tests its mutants
  ran, from its plan in `plans`, or :every when they ran every test."
  [{:keys [defs-in-scope loaded-once other-forms unreached results ignored stopped note plans]}]
  (let [reached (set (keep #(when (and (contains? #{:failed :timeout} (:outcome %)) (not (:at-load? %))) (:def %))
                           results))
        results (map #(cond-> %
                        (and (= :no-coverage (:outcome %)) (contains? reached (:def %))) (assoc :outcome :passed))
                     results)
        by-def (group-by :def results)
        counts (outcome-counts results)
        covered (+ (:killed counts) (:survived counts))
        scored (+ covered (:no-coverage counts))
        unreached-counts (frequencies (map :def unreached))]
    {:definitions (vec (for [def-name (distinct defs-in-scope)
                             :let [def-results (get by-def def-name)]
                             :when (seq def-results)]
                         (assoc (outcome-counts def-results)
                                :def def-name
                                :mutants (count def-results)
                                :tests (let [{:keys [kind tests]} (get plans def-name)]
                                         (cond
                                           (= :subset kind) (->> tests (map test-name) sort vec)
                                           kind :every
                                           :else [])))))
     :not-mutated (->> defs-in-scope
                       (remove (set (map :def ignored)))
                       (remove #(or (contains? by-def %) (contains? unreached-counts %)))
                       distinct
                       vec)
     :loaded-once (vec (distinct loaded-once))
     :other-forms (vec (distinct other-forms))
     :unreached (vec (for [def-name (distinct (map :def unreached))]
                       {:def def-name
                        :mutants (get unreached-counts def-name)}))
     :not-covered (vec (for [def-name (distinct defs-in-scope)
                             :when (some #(= :no-coverage (:outcome %)) (get by-def def-name))]
                         def-name))
     :note note
     :tests-run (update-vals (or plans {}) :kind)
     :totals (assoc counts :mutants (count results))
     :score (when (pos? scored) (double (/ (:killed counts) scored)))
     :covered-score (when (pos? covered) (double (/ (:killed counts) covered)))
     :mutants (mapv #(-> %
                         (select-keys [:def :row :col :from :at-load?])
                         (assoc :to (pr-str (:to %))
                                :outcome (outcome-key (:outcome %)))
                         (cond-> (:killed-by %) (assoc :killed-by (test-name (:killed-by %)))))
                    results)
     :ignored (mapv #(-> %
                         (select-keys [:def :row :col :from])
                         (assoc :to (pr-str (:to %))
                                :reason (:skip-reason %)))
                    ignored)
     :stopped stopped}))

(defn- print-list [title names]
  (when (seq names)
    (println (str "\n" title ": " (str/join ", " names)))))

(defn percent
  "Write a ratio as a whole percentage, or n/a when there is none."
  [ratio]
  (if ratio (format "%.0f%%" (* 100.0 ratio)) "n/a"))

(defn- print-report [{:keys [definitions not-mutated not-covered loaded-once other-forms unreached totals score
                             covered-score mutants ignored stopped note]}
                     source-lines
                     since
                     target]
  (println)
  (pprint/print-table [:def :mutants :killed :survived :no-coverage :unviable :timeout] definitions)
  (print-list "No coverage, no test calls them through their var" not-covered)
  (print-list (str "Not mutated, no operator " (if since "on the changed lines" "applies")) not-mutated)
  (print-list "Not mutated, loaded once" loaded-once)
  (print-list "Not mutated, not a definition" other-forms)
  (print-list "Not reached, the run stopped early" (map #(str (:def %) " (" (:mutants %) " mutants)") unreached))
  (doseq [[outcome title] [[:survived "Survived"]
                           [:unviable "Unviable, could not be read or compiled"]
                           [:timeout "Timed out"]]
          :let [shown (filter #(= outcome (:outcome %)) mutants)]
          :when (seq shown)]
    (println (str "\n" title ":"))
    (doseq [{:keys [def row col from to]} shown]
      (println (format "  %d:%d  %s -> %s  in %s" row col from to def))
      (println (str "      " (str/trim (nth source-lines (dec row) ""))))))
  ; mutate: skip first: any survivor makes a fine example
  (when-let [{:keys [row col]} (first (filter #(= :survived (:outcome %)) mutants))]
    (println (str "\nTo watch a new test fail on a survivor, apply it to the file, such as:\n"
                  "  clojure -M:test:mutate " target " --apply " row ":" col)))
  (when (seq ignored)
    (println "\nIgnored, with a skip marker:")
    (doseq [{:keys [def row col from to reason]} ignored]
      (println (format "  %d:%d  %s -> %s  in %s: %s" row col from to def reason))))
  (println (format "\n%d mutants: %d killed, %d survived, %d no coverage, %d unviable, %d timed out."
                   (:mutants totals) (:killed totals) (:survived totals) (:no-coverage totals) (:unviable totals)
                   (:timeout totals)))
  (println (str "Score " (percent score) ", covered score " (percent covered-score) "."))
  (when note
    (println (str "\nNote: " note)))
  (when stopped
    (println (str "\nStopped early: " stopped))))

(defn- form-lines [{:keys [loc]}]
  (let [[[start] [end]] (parser/position-span loc)]
    (range start (inc end))))

(defn scope
  "Return the definitions and mutants a run covers, the ones a skip marker leaves out, and the markers that match none.

  Return {:defs-in-scope … :loaded-once … :other-forms … :selected … :ignored … :unused-markers …}.
  The run stops when a definition named with `-d` does not exist, when a comment starts like a
  skip marker but is not one, and when a `:mutate/skip` anywhere in the file is no reason string
  or sits on a form the runner does not mutate. A marker is unused when no mutant of its line has its operator: on
  a full run anywhere in the file, and with `-d` or `-s` only on the lines in scope."
  [source {:keys [defs lines]}]
  (let [forms (top-level-forms source)
        wanted (set defs)
        unknown (remove (set (map :name forms)) wanted)]
    (when (seq unknown)
      (throw (setup-error (str "No top-level definition named " (str/join ", " (sort unknown)) "."))))
    (doseq [{:keys [loc name kind head]} forms
            :let [{:keys [reason]
                   :as found} (try
                                (skip-reason loc)
                                (catch Exception e
                                  (throw (setup-error (str "The metadata of " name " cannot be read: " (ex-message e))))))]
            :when found]
      (cond
        (not= :mutated kind)
        (throw (setup-error (str name " is a " head ", which the runner does not mutate, so its :mutate/skip does"
                                 " nothing. Remove it.")))

        (or (not (string? reason)) (str/blank? reason))
        (throw (setup-error (str "The :mutate/skip metadata on " name " needs a reason as a string, such as"
                                 " ^{:mutate/skip \"wraps a JDK call\"}.")))))
    (let [full-run? (and (empty? wanted) (not (set? lines)))
          in-scope (filter #(and (or (empty? wanted) (contains? wanted (:name %)))
                                 (or (not (set? lines)) (some lines (form-lines %))))
                           forms)
          of-kind (fn [kind] (filter #(= kind (:kind %)) in-scope))
          markers (skip-markers source)
          marker-reasons (into {} (map (juxt (juxt :row :operator) :reason)) markers)
          marked (fn [{:keys [row from]
                       :as mutant}]
                   (update mutant :skip-reason #(or % (marker-reasons [row from]))))
          in-scope-mutants (map marked (mutants in-scope))
          mutant-swaps (set (map (juxt :row :from) in-scope-mutants))
          in-scope-rows (set (mapcat form-lines in-scope))
          [ignored selected] ((juxt filter remove)
                              :skip-reason
                              (filter #(or (not (set? lines)) (contains? lines (:row %))) in-scope-mutants))]
      {:defs-in-scope (mapv :name (of-kind :mutated))
       :loaded-once (mapv #(str (:name %) " (" (:head %) ")") (of-kind :loaded-once))
       :other-forms (mapv :name (of-kind :other))
       :selected selected
       :ignored ignored
       :unused-markers (vec (for [{:keys [line row operator]} markers
                                  :when (and (or full-run? (contains? in-scope-rows row))
                                             (not (contains? mutant-swaps [row operator])))]
                              (str "line " line " (" operator ")")))})))

(defn- run-all
  "Run each mutant in turn, stopping at the first test run that keeps looping or the first restore that fails."
  [context selected]
  (loop [[mutant & more] selected
         results []]
    (if (nil? mutant)
      {:results results}
      (let [{:keys [stuck? restore-error plan-stuck?]
             :as result} (run-mutant context mutant)]
        (if plan-stuck?
          {:results results
           :unreached (cons mutant more)
           :stopped (str "the tests that call " (:def mutant) " keep running past the timeout without any mutation,"
                         " and would disturb every later run. Fix them, or raise --timeout.")}
          (let [results (conj results result)]
            (print (case (:outcome result) :passed "S" :no-coverage "N" :failed "." :unviable "U" :timeout "T"))
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
              :else (recur more results))))))))

(defn mutate-source
  "Mutate `target`, whose `source` is loaded from `file`, against `test-nss`, print the report, and return it.

  The tests must pass before any mutation, whether or not `timeout` is given. The report is
  also written to `report` when given."
  [target file source test-nss {:keys [defs since timeout report html all-tests]}]
  (let [lines (when since (lines-since file since))
        {:keys [defs-in-scope loaded-once other-forms selected ignored unused-markers]} (scope source {:defs defs
                                                                                                       :lines lines})
        _ (doseq [marker unused-markers]
            (println (str "Warning: the skip marker on " marker " matches no mutant; remove it or fix its operator.")))
        {:keys [ms covered note]} (when (seq selected)
                                    (baseline target (distinct (map :def selected)) test-nss timeout))
        timeout-ms (when ms (or timeout (default-timeout-ms ms)))
        _ (println (format "%d mutants of %s against %s%s%s%s"
                           (count selected) target (str/join ", " test-nss)
                           (if (seq ignored) (str ", " (count ignored) " ignored") "")
                           (if since (str ", lines changed since " since) "")
                           (cond
                             timeout (str ", timeout " timeout " ms")
                             timeout-ms (str ", timeout " timeout-ms " ms for every test, less for selected tests")
                             :else "")))
        plans (atom {})
        {:keys [results unreached stopped]} (run-all {:target target
                                                      :file (str file)
                                                      :test-nss test-nss
                                                      :timeout-ms timeout-ms
                                                      :fixed-timeout? (some? timeout)
                                                      :select? (not all-tests)
                                                      :covered covered
                                                      :plans plans}
                                                     selected)
        summary (summarize {:defs-in-scope defs-in-scope
                            :loaded-once loaded-once
                            :other-forms other-forms
                            :unreached unreached
                            :results results
                            :ignored ignored
                            :stopped stopped
                            :note note
                            :plans @plans})]
    (print-report summary (str/split-lines source) since target)
    (when report
      (spit report (with-out-str (pprint/pprint (assoc summary
                                                       :target target
                                                       :tests test-nss
                                                       :since since))))
      (println (str "Report written to " report)))
    (when html
      (spit html ((requiring-resolve 'cljs-patrol.dev.mutate-html/page) summary {:target target
                                                                                 :file (relative-path file)
                                                                                 :source source
                                                                                 :test-nss test-nss
                                                                                 :test-names (->> (test-vars-of test-nss)
                                                                                                  (map test-name)
                                                                                                  sort)
                                                                                 :since since}))
      (println (str "HTML report written to " html)))
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
  "Return 2 when the run stopped early, 1 when a mutant survived or had no coverage, and 0 otherwise."
  [{:keys [totals stopped]}]
  (cond
    stopped 2
    (pos? (+ (:survived totals 0) (:no-coverage totals 0))) 1
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
   ["-r" "--report FILE" "Write every mutant and its outcome to this EDN file"]
   [nil "--html FILE" "Write the report as an HTML page that leads with the test gaps and how to close them"]
   ["-a" "--all-tests" "Run every test for each mutant, as for a test that only reads state another test wrote"]
   [nil "--apply ROW:COL" "Write the mutant at this line and column into the file, and run nothing; needs no -t"]])

(def ^:private usage
  (str "Usage: clojure -M:test:mutate TARGET-NS -t TEST-NS [-t TEST-NS …] [-d DEF …] [-s REF] [--timeout MS]"
       " [-r FILE] [--html FILE] [-a]\n"
       "       clojure -M:test:mutate TARGET-NS --apply ROW:COL"))

(defn -main
  "Exit 0 when no mutant survives or lacks coverage, 1 when one does, and 2 when the run cannot start or stops early.

  With --apply, write that one mutant into the file and exit 0, or 2 when it cannot."
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
        target (some-> (first arguments) symbol)]
    (if (or errors (nil? target) (and (empty? (:test options)) (nil? (:apply options))))
      (do (println (str/join "\n" (concat errors [usage summary])))
          (System/exit 2))
      (try
        (when-let [position (:apply options)]
          (let [file (source-file target)]
            (println (apply-mutant file (slurp file) position))
            (System/exit 0)))
        (System/exit (exit-code (mutate target (:test options) {:defs (:def options)
                                                                :since (:since options)
                                                                :timeout (:timeout options)
                                                                :report (:report options)
                                                                :html (:html options)
                                                                :all-tests (:all-tests options)})))
        (catch Throwable e
          (println (if (::setup (ex-data e)) (ex-message e) (str "The run failed: " e)))
          (System/exit 2))))))
