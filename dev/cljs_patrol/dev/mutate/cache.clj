(ns cljs-patrol.dev.mutate.cache
  "Keep each mutant's outcome in a file, so the next run of the same code reuses it.

  A run that stopped goes on where it ended, --iterate runs only the gaps again after a change,
  and each run compares its gaps with the ones of the last run. A cached outcome is used only
  when the sources, the runner and the options are the same, as the key records; --iterate
  allows other sources."
  (:require
   [cljs-patrol.dev.mutate.apply :as apply]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def default-dir
  "Where the cache files go, one for each target namespace, in the directory the command runs from."
  ".mutate-cache")

(defn cache-file [dir target]
  (io/file dir (str target ".edn")))

(defn mutant-id
  "Name `mutant` the same way in each run of the same source, such as `holds-focus?/490:43/remove->filter`.

  The definition, the place and the swap name one mutant; the swap also tells a body mutant
  (`body->nil`) from a number or an operator at the same place."
  [{:keys [def row col from to]}]
  (str def "/" row ":" col "/" from "->" (pr-str to)))

(defn with-id [mutant]
  (assoc mutant :id (mutant-id mutant)))

(defn- hex [^java.security.MessageDigest digest]
  (format "%064x" (BigInteger. 1 (.digest digest))))

(defn text-hash [^String text]
  (hex (doto (java.security.MessageDigest/getInstance "SHA-256") (.update (.getBytes text "UTF-8")))))

(def source-roots
  "The files and trees, from the directory the command runs from, whose change makes a cached outcome stale.

  That is the code a target or a test may require, deps.edn, and the files tests read: the
  fixture projects, and the docs, whose examples are tests."
  ["deps.edn" "src" "test" "dev" "test-projects" "docs"])

(defn sources-hash
  "Hash the path and the bytes of every file of `source-roots` under `root`, in path order."
  [root]
  (let [root-path (.toPath (io/file root))
        digest (java.security.MessageDigest/getInstance "SHA-256")]
    (doseq [path (->> source-roots
                      (mapcat #(file-seq (io/file root %)))
                      (filter #(.isFile ^java.io.File %))
                      (map #(str (.relativize root-path (.toPath ^java.io.File %))))
                      sort)]
      (.update digest (.getBytes (str path "\n") "UTF-8"))
      (.update digest (java.nio.file.Files/readAllBytes (.resolve root-path ^String path))))
    (hex digest)))

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
  []
  (text-hash (str/join (map slurp (runner-files)))))

(defn run-key
  "Return what a cached outcome depends on: the hash of the sources, the runner's version, and the options."
  [{:keys [sources runner timeout all-tests]}]
  {:sources sources
   :runner runner
   :options {:all-tests (boolean all-tests)
             :timeout timeout}})

(defn- read-entry
  "Read the cache in `file`, or nil when there is none or it cannot be read, so the run starts over."
  [file]
  (let [entry (try (edn/read-string (slurp file)) (catch Exception _ nil))]
    (when (and (map? (:key entry)) (map? (:results entry)))
      entry)))

(defn- write-entry [file entry]
  (io/make-parents file)
  (apply/write-whole file (pr-str entry)))

(def ^:private gap-outcomes
  "The outcomes of a run, before the report names them, that are test gaps: survived and no coverage."
  #{:passed :no-coverage})

(defn- kept-by-iterate?
  "True when --iterate keeps a cached outcome: caught, or unviable, which no test changes; a loop is kept to run it last."
  [{:keys [outcome stuck?]}]
  (or (contains? #{:failed :unviable} outcome) (true? stuck?)))

(def ^:private key-parts
  [[:sources "the sources"]
   [:runner "the runner"]
   [:options "the options (-a or --timeout)"]])

(defn open!
  "Read the cache of the last run from `file`, and decide which of its outcomes this run, with `key`, reuses.

  Every outcome is kept when the key is the same. With `iterate` and the same runner and
  options, the caught and unviable ones and the loops are kept for each mutant id in
  `current-ids`, the mutants the target has now, also when the sources changed. With `fresh`,
  or another key, none is. An outcome --iterate kept is marked :trusted?, and only another run
  with `iterate` reuses it. A loop is kept only to run it last, never reused. Unless the key is
  the same, `file` is replaced at once with what this run keeps. With no `file`, nothing is
  read or written. `target` is the hash of the target's text: the last run's gaps are kept for
  the progress, also with `fresh`, when it is the same, since a mutant is named by its place."
  [{:keys [file key target current-ids fresh iterate]}]
  (let [old (when file (read-entry file))
        old-key (:key old)
        mode (cond
               fresh :fresh
               (and old (= old-key key)) :resume
               (and iterate old (= (dissoc old-key :sources) (dissoc key :sources))) :iterate
               :else :start-over)
        carried? (contains? #{:resume :iterate} mode)
        entry {:key key
               :target target
               :results (case mode
                          :resume (:results old)
                          :iterate (into {}
                                         (keep (fn [[id result]]
                                                 (when (and (contains? current-ids id) (kept-by-iterate? result))
                                                   [id (cond-> result (not (:stuck? result)) (assoc :trusted? true))])))
                                         (:results old))
                          {})
               :plans (if carried? (:plans old {}) {})
               :note (when carried? (:note old))}]
    (when (and file (not= :resume mode))
      (write-entry file entry))
    {:file file
     :iterate iterate
     :mode mode
     :key key
     :old-key old-key
     :entry (atom entry)
     :previous-gaps (when (and old (= (:target old) target))
                      (->> (:results old)
                           (keep (fn [[id {:keys [outcome]}]] (when (contains? gap-outcomes outcome) id)))
                           set))}))

(defn- start-message
  "Say what this run reuses of the last one, or why it reuses nothing, or nil when there was no last run."
  [{:keys [mode key old-key]} reused to-run]
  (let [reused-count (count reused)
        caught (->> reused vals (filter #(= :failed (:outcome %))) count)
        changed (keep (fn [[part words]] (when (not= (get old-key part) (get key part)) words)) key-parts)]
    (cond
      (= :fresh mode) "Running every mutant again, as --fresh asks."
      (and old-key (= :start-over mode)) (str "Running every mutant, since " (str/join " and " changed)
                                              " changed since the last run"
                                              (when (= ["the sources"] changed) "; --iterate keeps its caught mutants")
                                              ".")
      (zero? reused-count) nil
      (empty? to-run) (str "All " reused-count " mutants come from the last run, so no test runs.")
      :else (str "Reused the last run's outcome for " reused-count " of " (+ reused-count (count to-run)) " mutants"
                 (when (= :iterate mode)
                   (str " (" caught " caught, " (- reused-count caught) " did not compile), as --iterate asks"))
                 "; running the rest."
                 (when (= :iterate mode)
                   " --iterate trusts these outcomes, though the code changed, so run without it before a merge.")))))

(defn plan-run
  "Split `selected` into the outcomes this run reuses, by mutant id, and the mutants it runs, a past loop last.

  Return {:reused {id outcome} :to-run [mutant …] :message …}, where the message says what was
  reused, or is nil. An outcome only --iterate trusts runs again without `iterate`, so a plain
  run checks it. A mutant whose tests kept running in a past run runs again after every
  other one, so a loop that comes back stops the run with nothing else lost."
  [{:keys [entry iterate]
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
    {:reused reused
     :to-run to-run
     :message (start-message store reused to-run)}))

(defn- result-entry [{:keys [outcome killed-by at-load? stuck?]}]
  (cond-> {:outcome outcome}
    killed-by (assoc :killed-by (str (symbol killed-by)))
    at-load? (assoc :at-load? true)
    stuck? (assoc :stuck? true)))

(defn- plan-entries [plans]
  (update-vals plans (fn [{:keys [kind tests]}]
                       (cond-> {:kind kind}
                         (seq tests) (assoc :tests (mapv #(str (symbol %)) tests))))))

(defn record!
  "Add the outcome of the mutant `result`, the test `plans` so far and the baseline's `note` to the cache, and write it.

  It runs after each mutant, so a run that stops early, or crashes, keeps what it finished."
  [{:keys [file entry]} result plans note]
  (let [updated (swap! entry #(-> %
                                  (assoc-in [:results (:id result)] (result-entry result))
                                  (update :plans merge (plan-entries plans))
                                  (assoc :note note)))]
    (when file
      (write-entry file updated))))

(defn cached-plans
  "Return the test plans the cache holds, by definition, with test names for test vars."
  [{:keys [entry]}]
  (:plans @entry))

(defn cached-note
  "Return the note of the baseline the cache holds, for a run that runs no baseline."
  [{:keys [entry]}]
  (:note @entry))

(defn with-progress
  "Tag each gap of `summary` as :new or :still-open against the last run, and list the gaps since caught as :closed.

  A gap is :still-open when the last run also had it, and :new otherwise. Only a mutant this
  run has, and caught, counts as closed, so a run of fewer definitions closes none of the
  others. With no last run of the same target, `summary` stays as it is."
  [summary {:keys [previous-gaps]}]
  (if-not previous-gaps
    summary
    (let [gap? #(contains? #{:survived :no-coverage} (:outcome %))
          mutants (mapv #(cond-> %
                           (gap? %) (assoc :since-last-run (if (contains? previous-gaps (:id %)) :still-open :new)))
                        (:mutants summary))]
      (assoc summary
             :mutants mutants
             :progress {:closed (->> mutants
                                     (filter #(and (= :killed (:outcome %)) (contains? previous-gaps (:id %))))
                                     (mapv :id))
                        :new (count (filter #(= :new (:since-last-run %)) mutants))
                        :still-open (count (filter #(= :still-open (:since-last-run %)) mutants))}))))
