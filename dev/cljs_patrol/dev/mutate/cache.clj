(ns cljs-patrol.dev.mutate.cache
  "Keep each mutant's outcome in a file, so the next run of the same code reuses it.

  A run that stopped goes on where it ended, --iterate runs only the gaps again after a change,
  and each run compares its gaps with the ones of the last run. A cached outcome is used only
  when the sources, the test namespaces, the runner and the options are the same, as the key
  records; --iterate allows other sources. The run writes the whole file again after each
  outcome."
  (:require
   [cljs-patrol.dev.mutate.apply :as apply]
   [cljs-patrol.dev.mutate.git :as git]
   [cljs-patrol.parser :as parser]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

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

(defn- digest-hex [^java.security.MessageDigest digest]
  (format "%064x" (BigInteger. 1 (.digest digest))))

(defn bytes-hash
  "Hash `content`, a byte array, with SHA-256, as 64 hex digits."
  [^bytes content]
  (digest-hex (doto (java.security.MessageDigest/getInstance "SHA-256") (.update content))))

(defn text-hash
  "Hash `text` with SHA-256, as 64 hex digits."
  [^String text]
  (bytes-hash (.getBytes text "UTF-8")))

(defn- files-hash
  "Hash each of `files`, [name bytes] pairs, as its name, its length and its bytes, so no text can move between files."
  [files]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (doseq [[file-name ^bytes content] files]
      (.update digest (.getBytes (str file-name "\n" (alength content) "\n") "UTF-8"))
      (.update digest content))
    (digest-hex digest)))

(defn- definition-of [{:keys [wrapper form-loc]}]
  (or wrapper form-loc))

(defn with-ids
  "Add to each of `mutants` its :id, which the reports show, and its :place, which --iterate and the progress match.

  The place is the hash of the text of the mutant's top-level definition, its row and column
  from the definition's start, and its swap, so it stays when lines move above the definition
  and changes when the definition's text changes. Each definition is hashed once."
  [mutants]
  (into []
        (mapcat (fn [same-definition]
                  ; mutate: skip first: every mutant of the group has this definition
                  (let [definition (definition-of (first same-definition))
                        form-hash (text-hash (z/string definition))
                        [start-row start-col] (parser/position definition)]
                    (for [{:keys [row col from to]
                           :as mutant} same-definition]
                      (assoc mutant
                             :id (mutant-id mutant)
                             :place (str form-hash "/" (- row start-row) ":" (- col start-col) "/" from "->"
                                         (pr-str to)))))))
        (partition-by (comp parser/position definition-of) mutants)))

(def source-roots
  "The files and trees, from the directory the command runs from, whose change makes a cached outcome stale.

  That is the code a target or a test may require, deps.edn, the resources on the classpath,
  and the files tests read: the fixture projects, the docs and the README, whose examples are
  tests. The classpath directories under that directory count too, by [[classpath-roots]]."
  ["README.md" "deps.edn" "dev" "docs" "resources" "src" "test" "test-projects"])

(defn classpath-roots
  "Return the directories of `class-path`, as `java.class.path` writes it, that lie under `root`, relative to it.

  A jar, or a directory outside `root`, is left out: the key holds what the project's own
  files say."
  [class-path root]
  (let [root-path (.toPath (.getCanonicalFile (io/file root)))]
    (->> (str/split class-path (re-pattern (java.util.regex.Pattern/quote java.io.File/pathSeparator)))
         (map #(.getCanonicalFile (io/file %)))
         (filter #(.isDirectory ^java.io.File %))
         (map #(.toPath ^java.io.File %))
         (filter #(.startsWith ^java.nio.file.Path % root-path))
         (map #(let [relative (str (.relativize root-path %))]
                 (if (str/blank? relative) "." relative))))))

(defn- git-files
  "Return the paths under `roots` of `root` that git sees: tracked, or untracked and not ignored.

  Return nil when git fails or lists nothing, so the caller walks the files instead."
  [root roots]
  (try
    (let [{:keys [exit out]} (apply git/git (str root) "ls-files" "-z" "--cached" "--others" "--exclude-standard" "--"
                                    roots)]
      (when (zero? exit)
        (seq (remove str/blank? (str/split out #"\u0000")))))
    (catch Exception _ nil)))

(def ^:private never-hashed
  "The directories the walk outside git leaves out: git's own, and the cache's."
  #{".git" default-dir})

(defn- walked-files
  "Return the paths of every file under `roots` of `root`, for a directory git does not know, but in `never-hashed`."
  [root roots]
  (let [root-path (.toPath (.getCanonicalFile (io/file root)))]
    (->> roots
         (mapcat #(file-seq (.getCanonicalFile (io/file root %))))
         (filter #(.isFile ^java.io.File %))
         (map #(.relativize root-path (.toPath ^java.io.File %)))
         (remove #(contains? never-hashed (str (.getName ^java.nio.file.Path % 0))))
         (map str))))

(defn sources-hash
  "Hash the path and the bytes of every file under `roots` of `root` that git sees, in path order.

  Git leaves out ignored files, such as `.DS_Store` or a generated report. Outside git, every
  file counts."
  [root roots]
  (try
    (let [root-path (.toPath (io/file root))]
      (->> (or (git-files root roots) (walked-files root roots))
           (filter #(.isFile (io/file root %)))
           distinct
           sort
           (map (fn [path] [path (java.nio.file.Files/readAllBytes (.resolve root-path ^String path))]))
           files-hash))
    (catch Exception e
      (throw (ex-info (str "reading the sources under " root " failed: " (ex-message e)) {} e)))))

(def ^:private runner-src-files
  "The files under src/ that the runner reads forms through: the parser and what it requires."
  ["cljs_patrol/fs.clj" "cljs_patrol/group.clj" "cljs_patrol/parser.clj" "cljs_patrol/timings.clj"])

(defn runner-files
  "Find the runner's source files on the classpath, and the src files it reads forms through.

  They are the command line, each namespace under `cljs-patrol.dev.mutate.`, and the parser with
  what it requires. The page code is left out, since it changes no outcome."
  []
  (let [command-line (io/file (io/resource "cljs_patrol/dev/mutate.clj"))]
    (concat [command-line]
            (->> (.listFiles (io/file (.getParentFile command-line) "mutate"))
                 (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))
                 (sort-by #(.getName ^java.io.File %)))
            (map #(io/file (io/resource %)) runner-src-files))))

(defn runner-version
  "Hash the runner's source files, so a change to the runner makes the next run start over, also with --iterate."
  ([] (runner-version runner-files))
  ([find-files]
   (try
     (files-hash (map (fn [^java.io.File file] [(.getName file) (java.nio.file.Files/readAllBytes (.toPath file))])
                      (find-files)))
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

(defn project-key
  "Make the key of a run in `root`, the directory the command runs from, against `test-nss` with `options`.

  The sources are every file of [[source-roots]] and of the classpath directories under `root`."
  [root test-nss {:keys [timeout all-tests]}]
  (run-key {:sources (sources-hash root (distinct (concat source-roots
                                                          (classpath-roots (System/getProperty "java.class.path")
                                                                           root))))
            :test-nss test-nss
            :runner (runner-version)
            :timeout timeout
            :all-tests all-tests}))

(defn- warn [message]
  (println (str "Warning: " message)))

(defn- read-entry
  "Read the cache in `file`, or nil when there is none, or, with a warning, when it cannot be read."
  [file]
  (when (.exists (io/file file))
    (try
      (let [entry (edn/read-string (slurp file))]
        (if (and (map? (:key entry))
                 (map? (:results entry))
                 (every? #(let [value (get entry %)] (or (nil? value) (map? value))) [:notes :plans :last-run])
                 (every? (fn [result]
                           (and (map? result)
                                (contains? #{:failed :passed :no-coverage :unviable :timeout} (:outcome result))
                                (string? (:place result))))
                         (vals (:results entry))))
          entry
          (throw (ex-info "it holds no cache" {}))))
      (catch Exception e
        (warn (str "the cache " file " could not be read, so the run does not use it; its first outcome replaces it: "
                   (ex-message e)))))))

(defn- write!
  "Write what the store holds to its file, with `last-run` as the last run's outcomes, unless an earlier write failed.

  A write that fails prints one warning, and the run goes on without the cache."
  [{:keys [file entry]} last-run]
  (when-let [path @file]
    (try
      (io/make-parents path)
      (apply/write-whole path (binding [*print-length* nil
                                        *print-level* nil]
                                (pr-str (assoc @entry :last-run last-run))))
      (catch Exception e
        (warn (str "the cache " path " could not be written, so the run goes on without it: " (ex-message e)))
        (reset! file nil)))))

(def ^:private gap-outcomes
  "The outcomes, as the report names them, that are test gaps."
  #{:survived :no-coverage})

(defn- kept-by-iterate?
  "True when --iterate keeps a cached outcome: caught, or unviable, which no test changes; a loop is kept to run it last."
  [{:keys [outcome stuck?]}]
  (or (contains? #{:failed :unviable} outcome) (true? stuck?)))

(defn- iterate-results
  "Return the outcomes of `old-results` that --iterate keeps, under the id each mutant has now, by `current-places`.

  `current-places` maps the :place of each mutant the target has now to its id. An outcome whose
  place no mutant has now is dropped, and a kept one that is no loop is marked :trusted? and loses the name of the
  test that caught it, which may be gone."
  [old-results current-places]
  (into {}
        (keep (fn [[_ {:keys [place stuck?]
                       :as result}]]
                (when-let [id (get current-places place)]
                  (when (kept-by-iterate? result)
                    [id (cond-> result (not stuck?) (-> (dissoc :killed-by) (assoc :trusted? true)))]))))
        old-results))

(def ^:private key-parts
  [[:sources "the sources"]
   [:test-nss "the test namespaces"]
   [:runner "the runner"]
   [:options "the options (-a or --timeout)"]])

(defn open!
  "Read the cache of the last run from `file`, and decide which of its outcomes this run reuses.

  `make-key` gives this run's key. Every outcome is kept when the key is the same. With
  `iterate` and the same test namespaces, runner and options, the caught and unviable ones and
  the loops are kept for each mutant whose definition's text and place in it are the same, by
  `current-places`, a function called only then, also when the sources changed. With `fresh`, or another key, none is. An
  outcome --iterate kept is marked :trusted?, and only another run with `iterate` reuses it. A
  loop is kept only to run it last, never reused. Only a resumed run keeps the test plans and
  the notes. Nothing is written until [[record!]] or [[finish!]]. With no `file`, or when the
  key cannot be made, nothing is read or written. The outcomes the last run reported, by place,
  are kept as :previous for the progress."
  [{:keys [file make-key current-places fresh iterate]}]
  (let [key (when file
              (try (make-key)
                   (catch Exception e
                     (warn (str "the cache key could not be made, so the run does not use the cache: "
                                (ex-message e))))))
        file (when key file)
        old (when file (read-entry file))
        old-key (:key old)
        mode (cond
               fresh :fresh
               (and old (= old-key key)) :resume
               (and iterate old (= (dissoc old-key :sources) (dissoc key :sources))) :iterate
               :else :start-over)
        resume? (= :resume mode)]
    {:file (atom file)
     :iterate iterate
     :mode mode
     :key key
     :old-key old-key
     :seen-plans (atom {})
     :recorded? (atom false)
     :entry (atom {:key key
                   :results (case mode
                              :resume (:results old)
                              :iterate (iterate-results (:results old) (current-places))
                              {})
                   :plans (if resume? (:plans old {}) {})
                   :notes (if resume? (:notes old {}) {})})
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
      (empty? to-run) (str (if (= 1 reused-count) "The 1 mutant comes" (str "All " reused-count " mutants come"))
                           " from the last run"
                           (if (= :iterate mode)
                             ", as --iterate asks; the tests run once to find which tests reach each definition."
                             ", so no test runs.")
                           trust-note)
      :else (str "Reused the last run's outcome for " reused-count " of " (+ reused-count (count to-run)) " mutants"
                 (when (= :iterate mode)
                   (str " (" caught " caught, " (- reused-count caught) " did not compile), as --iterate asks"))
                 "; running the rest." trust-note))))

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

(defn stop-note
  "Say what the cache keeps for the next run after a run that stopped for `reason`, as `run/run-all` names it.

  `recorded?` is true when the run recorded an outcome before it stopped."
  [reason recorded?]
  (case reason
    :stuck " The cache keeps the mutants that finished, so the next run runs the rest, and this one last."
    :plan-stuck (str (if recorded?
                       " The cache keeps the mutants that finished, but the next run stops here again"
                       " The next run stops here again")
                     " until these tests stop looping.")
    :restore-error " The cache keeps the mutants that finished, this one too, so the next run runs the rest."
    :files-changed " The cache has no outcome of this mutant, so the next run runs it again."
    :files-changed-at-end (str " The cache keeps every mutant of this run, so a run without --fresh runs no test."
                               " Put the files back, and fix the test before you run with --fresh.")
    :files-changed-unnamed (str " The cache keeps every mutant of this run, so a run without --fresh runs no test."
                                " Put the files back, and run with --fresh, and -d for one definition at a time,"
                                " to find the mutant.")
    nil))

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

(defn record!
  "Add the outcome of the mutant `result`, the test `plans` so far and the baseline's `note` to the cache, and write it.

  The file is written again after each outcome, so a run that stops early, or is killed, keeps
  every outcome it finished. The last run's outcomes stay as they were until [[finish!]]. Only
  the plans that changed since the last outcome are turned into test names."
  [{:keys [entry previous seen-plans recorded?]
    :as store}
   result
   plans
   note]
  (let [changed (into {} (remove (fn [[def-name plan]] (identical? plan (get @seen-plans def-name)))) plans)]
    (reset! seen-plans plans)
    (swap! entry #(-> %
                      (assoc-in [:results (:id result)] (result-entry result))
                      (update :plans merge (plan-entries changed))
                      (cond-> note (assoc-in [:notes (:def result)] note)))))
  (reset! recorded? true)
  (write! store previous))

(defn finish!
  "Record `last-run`, the outcomes by place that the run reported, as the last run, and the test `plans` the report used.

  Every run that prints its report writes it, in every mode, so a run that reused every outcome
  still saves its key, what it reported and the tests of each definition, which a later run
  that runs no baseline shows. A run that reported nothing, such as one whose scope is empty,
  leaves the file as it was."
  [{:keys [entry]
    :as store}
   last-run
   plans]
  (when (seq last-run)
    (swap! entry update :plans merge (plan-entries plans))
    (write! store last-run)))

(defn cached-plans
  "Return the test plans the cache holds, by definition, with test names for test vars."
  [{:keys [entry]}]
  (:plans @entry))

(defn run-note
  "Join this run's baseline `note` with the notes that the reused outcomes of `def-names` ran under, or nil when none."
  [{:keys [entry]} note def-names]
  (some->> (concat (keep (:notes @entry) def-names) [note]) (remove nil?) distinct seq (str/join " ")))

(defn project-cache
  "Return the cache of `target` in `dir` for a run in `root` against `test-nss` with `options`, or nil without a `dir`."
  [dir root target test-nss options]
  (when dir
    {:file (cache-file dir target)
     :make-key #(project-key root test-nss options)}))

(defn with-progress
  "Tag each gap of `summary` against the outcomes the last run reported, and list its gaps now caught as :closed.

  Mutants are matched by `places`, each id's place, so an edit keeps the tags of the definitions
  it does not touch. A gap is :still-open when the last run had it as a gap, and :new when the
  last run had it and caught it or saw no gap. A gap the last run did not have gets no tag,
  and is counted as :not-run-before. With no last run, `summary` stays as it is."
  [summary {:keys [previous]} places]
  (if-not previous
    summary
    (let [gap? #(contains? gap-outcomes (:outcome %))
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
