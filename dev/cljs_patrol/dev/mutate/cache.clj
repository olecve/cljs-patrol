(ns cljs-patrol.dev.mutate.cache
  "Keep each mutant's outcome in a file, so the next run of the same code reuses it.

  A run that stopped goes on where it ended, --iterate runs only the gaps again after a change,
  and each run compares its gaps with the ones of the last run. A cached outcome is used only
  when the sources, the test namespaces, the runner and the options are the same, as the key
  records; --iterate allows other sources.

  A run appends each outcome to a log as soon as it is known, and folds the log into the cache
  at its end. A run that finds a log left over, from a run that was killed, folds it first."
  (:require
   [cljs-patrol.dev.mutate.apply :as apply]
   [cljs-patrol.dev.mutate.forms :as forms]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def default-dir
  "Where the cache files go, one for each target namespace, in the directory the command runs from."
  ".mutate-cache")

(defn cache-file [dir target]
  (io/file dir (str target ".edn")))

(defn log-file
  "Return the log beside the cache `file`, which a run appends each outcome to: `<ns>.log.edn` for `<ns>.edn`."
  [file]
  (io/file (str/replace (str file) #"\.edn$" ".log.edn")))

(defn mutant-id
  "Name `mutant` the same way in each run of the same source, such as `holds-focus?/490:43/remove->filter`.

  The definition, the place and the swap name one mutant; the swap also tells a body mutant
  (`body->nil`) from a number or an operator at the same place."
  [{:keys [def row col from to]}]
  (str def "/" row ":" col "/" from "->" (pr-str to)))

(defn mutant-place
  "Name `mutant` by the text of its top-level definition and its place in it, so a run finds it after lines move.

  The place is the row and column from the definition's start. A change to the definition's
  text, such as a skip marker inside it, gives each of its mutants a new name."
  [{:keys [form-hash form-start row col from to]}]
  (let [[start-row start-col] form-start]
    (str form-hash "/" (- row start-row) ":" (- col start-col) "/" from "->" (pr-str to))))

(defn with-id
  "Add the mutant's :id, which the reports show, and its :place, which --iterate and the progress match."
  [mutant]
  (assoc mutant
         :id (mutant-id mutant)
         :place (mutant-place mutant)))

(def text-hash forms/text-hash)

(def source-roots
  "The files and trees, from the directory the command runs from, whose change makes a cached outcome stale.

  That is the code a target or a test may require, deps.edn, the resources on the classpath,
  and the files tests read: the fixture projects, the docs and the README, whose examples are
  tests."
  ["README.md" "deps.edn" "dev" "docs" "resources" "src" "test" "test-projects"])

(defn sources-hash
  "Hash the path and the bytes of every file of `source-roots` under `root`, in path order."
  [root]
  (try
    (let [root-path (.toPath (io/file root))
          digest (java.security.MessageDigest/getInstance "SHA-256")]
      (doseq [path (->> source-roots
                        (mapcat #(file-seq (io/file root %)))
                        (filter #(.isFile ^java.io.File %))
                        (map #(str (.relativize root-path (.toPath ^java.io.File %))))
                        sort)]
        (.update digest (.getBytes (str path "\n") "UTF-8"))
        (.update digest (java.nio.file.Files/readAllBytes (.resolve root-path ^String path))))
      (format "%064x" (BigInteger. 1 (.digest digest))))
    (catch Exception e
      (throw (ex-info (str "reading the sources under " root " failed: " (ex-message e)) {} e)))))

(defn runner-files
  "Find the runner's source files on the classpath: the command line and each namespace under `cljs-patrol.dev.mutate.`.

  The page code is left out, since it changes no outcome."
  []
  (let [command-line (io/file (io/resource "cljs_patrol/dev/mutate.clj"))]
    (cons command-line
          (->> (.listFiles (io/file (.getParentFile command-line) "mutate"))
               (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))
               (sort-by #(.getName ^java.io.File %))))))

(defn runner-version
  "Hash the runner's source files, so a change to the runner makes the next run start over, also with --iterate."
  ([] (runner-version runner-files))
  ([find-files]
   (try
     (text-hash (str/join (map slurp (find-files))))
     (catch Exception e
       (throw (ex-info (str "reading the runner files failed: " (ex-message e)) {} e))))))

(defn run-key
  "Return what a cached outcome depends on: the sources, the test namespaces the run names, the runner, and the options."
  [{:keys [sources test-nss runner timeout all-tests]}]
  {:sources sources
   :test-nss (->> test-nss (map str) sort vec)
   :runner runner
   :options {:all-tests (boolean all-tests)
             :timeout timeout}})

(defn- warn [message]
  (println (str "Warning: " message)))

(defn- unreadable [file e]
  (warn (str "the cache " file " could not be read, so the run starts over: " (ex-message e))))

(defn- read-entry
  "Read the cache in `file`, or nil when there is none, or, with a warning, when it cannot be read."
  [file]
  (when (.exists (io/file file))
    (try
      (let [entry (edn/read-string (slurp file))]
        (if (and (map? (:key entry)) (map? (:results entry)))
          entry
          (throw (ex-info "it holds no cache" {}))))
      (catch Exception e
        (unreadable file e)))))

(defn- write-entry [file entry]
  (io/make-parents file)
  (apply/write-whole file (pr-str entry)))

(defn- result-entry [{:keys [outcome killed-by at-load? stuck? place]}]
  (cond-> {:outcome outcome
           :place place}
    killed-by (assoc :killed-by (str (symbol killed-by)))
    at-load? (assoc :at-load? true)
    stuck? (assoc :stuck? true)))

(defn- plan-entries [plans]
  (update-vals plans (fn [{:keys [kind tests]}]
                       (cond-> {:kind kind}
                         (seq tests) (assoc :tests (mapv #(str (symbol %)) tests))))))

(defn- add-note [entry note]
  (cond-> entry
    (not-any? #{note} (:notes entry)) (update :notes conj note)))

(defn- apply-line
  "Add one line of a log, a :result, :plans or :note, to `entry` and to `ran`, the outcomes by place."
  [[entry ran] {:keys [id result plans note]}]
  [(cond-> entry
     result (assoc-in [:results id] result)
     plans (update :plans merge plans)
     note (add-note note))
   (cond-> ran
     result (assoc (:place result) (:outcome result)))])

(defn- fold-log!
  "Fold a log that a killed run left beside `file` into `file`, and delete the log.

  The log's first line holds the cache as that run opened it, so the log is folded under that
  run's own key, whatever the key of this run is. A line that cannot be read ends the fold, with a
  warning, and the lines before it count. A log that cannot be deleted gives a warning."
  [file]
  (let [log (log-file file)]
    (when (.exists log)
      (let [[header & lines] (try (str/split-lines (slurp log)) (catch Exception e (unreadable log e) nil))
            start (try (some-> header edn/read-string) (catch Exception e (unreadable log e) nil))]
        (when (map? (:base start))
          (let [[entry ran] (reduce (fn [folded line]
                                      (if-let [value (try (edn/read-string line) (catch Exception e (unreadable log e) nil))]
                                        (apply-line folded value)
                                        (reduced folded)))
                                    [(:base start) (:ran start)]
                                    lines)]
            (write-entry file (assoc entry :last-run ran)))))
      (try
        (io/delete-file log)
        (catch Exception e
          (warn (str "the cache " log " could not be deleted, so the next run reads it again: " (ex-message e))))))))

(def ^:private gap-outcomes
  "The outcomes of a run, before the report names them, that are test gaps: survived and no coverage."
  #{:passed :no-coverage})

(defn- kept-by-iterate?
  "True when --iterate keeps a cached outcome: caught, or unviable, which no test changes; a loop is kept to run it last."
  [{:keys [outcome stuck?]}]
  (or (contains? #{:failed :unviable} outcome) (true? stuck?)))

(defn- iterate-results
  "Return the outcomes of `old-results` that --iterate keeps, under the id each mutant has now, by `current-places`.

  `current-places` maps the :place of each mutant the target has now to its id. An outcome whose
  place no mutant has now is dropped, and a kept one that is no loop is marked :trusted?."
  [old-results current-places]
  (into {}
        (keep (fn [[_ {:keys [place stuck?]
                       :as result}]]
                (when-let [id (get current-places place)]
                  (when (kept-by-iterate? result)
                    [id (cond-> result (not stuck?) (assoc :trusted? true))]))))
        old-results))

(def ^:private key-parts
  [[:sources "the sources"]
   [:test-nss "the test namespaces"]
   [:runner "the runner"]
   [:options "the options (-a or --timeout)"]])

(defn open!
  "Read the cache of the last run from `file`, and decide which of its outcomes this run reuses.

  A log a killed run left is folded in first. `make-key` gives this run's key. Every outcome is
  kept when the key is the same. With `iterate` and the same test namespaces, runner and
  options, the caught and unviable ones and the loops are kept for each mutant whose
  definition's text and place in it are the same, by `current-places`, also when the sources
  changed. With `fresh`, or another key, none is. An outcome --iterate kept is marked
  :trusted?, and only another run with `iterate` reuses it. A loop is kept only to run it last,
  never reused. Only a resumed run keeps the test plans and the notes. Nothing is written until
  [[record!]]. With no `file`, or when the key cannot be made, nothing is read or written. The
  outcomes the last run reported, by place, are kept as :previous for the progress."
  [{:keys [file make-key current-places fresh iterate]}]
  (let [key (when file
              (try (make-key)
                   (catch Exception e
                     (warn (str "the cache key could not be made, so the run goes on without the cache: "
                                (ex-message e))))))
        file (when key file)
        old (when file
              (fold-log! file)
              (read-entry file))
        old-key (:key old)
        mode (cond
               fresh :fresh
               (and old (= old-key key)) :resume
               (and iterate old (= (dissoc old-key :sources) (dissoc key :sources))) :iterate
               :else :start-over)
        resume? (= :resume mode)
        entry {:key key
               :results (case mode
                          :resume (:results old)
                          :iterate (iterate-results (:results old) current-places)
                          {})
               :plans (if resume? (:plans old {}) {})
               :notes (if resume? (:notes old []) [])}]
    {:file (atom file)
     :iterate iterate
     :mode mode
     :key key
     :old-key old-key
     :base entry
     :entry (atom entry)
     :ran (atom {})
     :logged (atom nil)
     :previous (:last-run old)}))

(defn- start-message
  "Say what this run reuses of the last one, or why it reuses nothing, or nil when there was no last run."
  [{:keys [mode key old-key]} reused to-run]
  (let [reused-count (count reused)
        caught (->> reused vals (filter #(= :failed (:outcome %))) count)
        changed (keep (fn [[part words]] (when (not= (get old-key part) (get key part)) words)) key-parts)
        trust-note (when (some :trusted? (vals reused))
                     (str " --iterate trusts the caught outcomes, though the code changed; a run without it checks"
                          " them, so run one before a merge."))]
    (cond
      (= :fresh mode) "Running every mutant again, as --fresh asks."
      (and old-key (= :start-over mode)) (str "Running every mutant, since " (str/join " and " changed)
                                              " changed since the last run"
                                              (when (= ["the sources"] changed) "; --iterate keeps its caught mutants")
                                              ".")
      (zero? reused-count) nil
      (empty? to-run) (str "All " reused-count " mutants come from the last run"
                           (if (= :iterate mode)
                             ", as --iterate asks; the tests run once to plan which tests reach each definition."
                             ", so no test runs.")
                           trust-note)
      :else (str "Reused the last run's outcome for " reused-count " of " (+ reused-count (count to-run)) " mutants"
                 (when (= :iterate mode)
                   (str " (" caught " caught, " (- reused-count caught) " did not compile), as --iterate asks"))
                 "; running the rest." trust-note))))

(defn plan-run
  "Split `selected` into the outcomes this run reuses, by mutant id, and the mutants it runs, a past loop last.

  Return {:reused {id outcome} :to-run [mutant …] :message …}, where the message says what was
  reused, or is nil. The reused outcomes count as outcomes this run reports, for the progress
  of the next run. An outcome only --iterate trusts runs again without `iterate`, so a plain
  run checks it. A mutant whose tests kept running in a past run runs again after every
  other one, so a loop that comes back stops the run with nothing else lost."
  [{:keys [entry iterate ran]
    :as store}
   selected]
  (let [results (:results @entry)
        reused (into {}
                     (keep #(when-let [{:keys [stuck? trusted?]
                                        :as result} (get results (:id %))]
                              (when (and (not stuck?) (or iterate (not trusted?))) [(:id %) result])))
                     selected)
        [later now] ((juxt filter remove) #(:stuck? (get results (:id %)))
                                          (remove #(contains? reused (:id %)) selected))
        to-run (into (vec now) later)]
    (reset! ran (into {} (map (fn [[_ {:keys [place outcome]}]] [place outcome])) reused))
    {:reused reused
     :to-run to-run
     :message (start-message store reused to-run)}))

(defn stop-note
  "Say what the cache keeps for the next run after a run that stopped for `reason`, as `run/run-all` names it."
  [reason]
  (case reason
    :stuck " The cache keeps the mutants that finished, so the next run runs the rest, and this one last."
    :plan-stuck (str " The cache keeps the mutants that finished, but the next run stops here again until these tests"
                     " stop looping.")
    :restore-error " The cache keeps the mutants that finished, this one too, so the next run runs the rest."
    nil))

(defn- append-lines!
  "Append `values` to the log of the cache, one EDN line each, and start the log with `header` when it is new.

  A write that fails prints one warning, and the run goes on without the cache."
  [{:keys [file logged]} header values]
  (when-let [path @file]
    (try
      (let [log (log-file path)
            new? (nil? @logged)]
        (io/make-parents log)
        (spit log (str/join (map #(str (pr-str %) "\n") (cond->> values new? (cons header)))) :append (not new?))
        (reset! logged true))
      (catch Exception e
        (warn (str "the cache " path " failed, so the run goes on without it: " (ex-message e)))
        (reset! file nil)))))

(defn record!
  "Add the outcome of the mutant `result`, the test `plans` so far and the baseline's `note` to the cache.

  The outcome goes to the log at once, as one line, so a run that is killed keeps every outcome
  it finished."
  [{:keys [entry ran base]
    :as store}
   result
   plans
   note]
  (let [header {:key (:key base)
                :base base
                :ran @ran}
        line {:id (:id result)
              :result (result-entry result)}
        plan-lines (let [changed (into {}
                                       (remove (fn [[def-name plan]] (= plan (get-in @entry [:plans def-name]))))
                                       (plan-entries plans))]
                     (when (seq changed)
                       [{:plans changed}]))
        note-lines (when (and note (not-any? #{note} (:notes @entry)))
                     [{:note note}])
        lines (concat [line] plan-lines note-lines)
        [updated now-ran] (reduce apply-line [@entry @ran] lines)]
    (reset! entry updated)
    (reset! ran now-ran)
    (append-lines! store header lines)))

(defn finish!
  "Write the outcomes of the run and its test `plans` into the cache file and delete its log, when it recorded any.

  A run that recorded none leaves the file as it was, so a run that stops before its first
  outcome keeps the last run's cache. The outcomes this run reported, by place, become the last
  run that the next run's progress compares with."
  [{:keys [file logged entry ran]} plans]
  (when-let [path (when @logged @file)]
    (try
      (write-entry path (-> @entry
                            (update :plans merge (plan-entries plans))
                            (assoc :last-run @ran)))
      (io/delete-file (log-file path))
      (catch Exception e
        (warn (str "the cache " path " failed, so the run goes on without it: " (ex-message e)))))))

(defn cached-plans
  "Return the test plans the cache holds, by definition, with test names for test vars."
  [{:keys [entry]}]
  (:plans @entry))

(defn run-note
  "Join this run's baseline `note` with the notes the reused outcomes ran under, or nil when there is none."
  [{:keys [entry]} note]
  (some->> (cond-> (:notes @entry) note (conj note)) distinct seq (str/join " ")))

(defn with-progress
  "Tag each gap of `summary` against the outcomes the last run reported, and list its gaps now caught as :closed.

  Mutants are matched by `places`, each id's place, so an edit keeps the tags of the definitions
  it does not touch. A gap is :still-open when the last run had it as a gap, and :new when the
  last run had it and caught it or saw no gap. A gap the last run did not have gets no tag,
  and is counted as :not-run-before. With no last run, `summary` stays as it is."
  [summary {:keys [previous]} places]
  (if-not previous
    summary
    (let [gap? #(contains? #{:survived :no-coverage} (:outcome %))
          last-outcome #(get previous (get places (:id %)))
          was-gap? #(contains? gap-outcomes (last-outcome %))
          mutants (mapv #(cond-> %
                           (and (gap? %) (last-outcome %))
                           (assoc :since-last-run (if (was-gap? %) :still-open :new)))
                        (:mutants summary))]
      (assoc summary
             :mutants mutants
             :progress {:closed (->> mutants
                                     (filter #(and (= :killed (:outcome %)) (was-gap? %)))
                                     (mapv :id))
                        :new (count (filter #(= :new (:since-last-run %)) mutants))
                        :still-open (count (filter #(= :still-open (:since-last-run %)) mutants))
                        :not-run-before (count (filter #(and (gap? %) (nil? (:since-last-run %))) mutants))}))))
