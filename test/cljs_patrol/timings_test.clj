(ns cljs-patrol.timings-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.group :as group]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.timings :as timings]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]])
  (:import
   (java.util.concurrent.atomic
    AtomicLong)
   (java.util.function
    LongSupplier)))

(defn- manual-clock
  "A clock that reads `time` and moves only when the test sets it."
  [^AtomicLong time]
  (reify LongSupplier
    (getAsLong [_] (.get time))))

(defn- ticking-clock
  "A clock that moves one nanosecond each time it is read."
  []
  (let [time (AtomicLong.)]
    (reify LongSupplier
      (getAsLong [_] (.incrementAndGet time)))))

(defn- fake-group [id handlers]
  (reify group/RuleGroup
    (group-id [_] id)
    (group-name [_] (name id))
    (parse-handlers [_] handlers)
    (analyze [_ _] {})
    (summary-lines [_ _] [])
    (failed? [_ _] false)
    (suggestions [_] {})
    (docs-anchors [_] {})
    (rule->tier [_] {})
    (file-extensions [_] #{".cljs"})))

(defn- temp-source-file [source]
  (let [file (java.io.File/createTempFile "timings" ".cljs")]
    (spit file source)
    file))

(deftest summary-totals-phases-groups-and-files
  (let [time (AtomicLong. 0)
        recorder (timings/recorder (manual-clock time))]
    (timings/record-phase! recorder :discovery 5)
    (is (= :analyzed (timings/timed recorder :analyze #(do (.set time 7) :analyzed)))
        "timed returns what f returns")
    (timings/record-file! recorder {:file "a.cljs"
                                    :handlers {:x 3
                                               :y 4}
                                    :parse 2
                                    :walk 10})
    (timings/record-file! recorder {:file "b.cljs"
                                    :handlers {:x 6}
                                    :parse 1
                                    :walk 20})
    (timings/record-analyze! recorder :y 1)
    (timings/record-analyze! recorder :a 4)
    (timings/record-analyze! recorder :a 5)
    (.set time 50)
    (timings/start-phase! recorder :report)
    (.set time 60)
    (let [{:keys [phases total groups files]} (timings/summary recorder)]
      (is (= {:analyze 7
              :discovery 5
              :parse 3
              :report 10
              :walk 30}
             phases)
          "file times add up into parse and walk, and the open report phase closes at the summary")
      (is (= 60 total)
          "total runs from recorder creation to summary")
      (is (= [{:analyze 9
               :group :a
               :handlers 0
               :total 9}
              {:analyze 0
               :group :x
               :handlers 9
               :total 9}
              {:analyze 1
               :group :y
               :handlers 4
               :total 5}]
             groups)
          "groups slowest first, ties by name")
      (is (= [{:file "b.cljs"
               :handlers 6
               :parse 1
               :total 21}
              {:file "a.cljs"
               :handlers 7
               :parse 2
               :total 12}]
             files)
          "files slowest first, total being parse plus walk"))))

(deftest report-lines-lay-out-tables
  (is (= ["Timings (parse, walk and handlers add up over files):"
          "  phase       time  share"
          "  discovery  0.10s     5%"
          "  parse      0.50s    25%"
          "  walk       1.20s    60%"
          "  report     0.10s     5%"
          "  total      2.00s"
          ""
          "  group  handlers  analyze  total"
          "  a11y      1.00s    0.10s  1.10s"
          "  tests     0.20s    0.00s  0.20s"
          ""
          "  slowest files  total  parse  handlers"
          "  src/b.cljs     1.00s  0.30s     0.60s"]
         (timings/report-lines {:files [{:file "src/b.cljs"
                                         :handlers 600000000
                                         :parse 300000000
                                         :total 1000000000}
                                        {:file "src/a.cljs"
                                         :handlers 100000000
                                         :parse 100000000
                                         :total 200000000}]
                                :groups [{:analyze 100000000
                                          :group :a11y
                                          :handlers 1000000000
                                          :total 1100000000}
                                         {:analyze 0
                                          :group :tests
                                          :handlers 200000000
                                          :total 200000000}]
                                :phases {:discovery 100000000
                                         :parse 500000000
                                         :report 100000000
                                         :walk 1200000000}
                                :total 2000000000}
                               1))
      "phases in run order, only those recorded, and only top-files slowest files"))

(deftest report-lines-without-files-or-time
  (is (= ["Timings (parse, walk and handlers add up over files):"
          "  phase       time  share"
          "  discovery  0.00s      -"
          "  total      0.00s"
          ""
          "  group  handlers  analyze  total"]
         (timings/report-lines {:files []
                                :groups []
                                :phases {:discovery 0}
                                :total 0}
                               10))
      "no share of a zero total, no rows for phases not recorded, and no files table without files"))

(deftest timed-handler-adds-to-its-slot
  (let [recorder (timings/recorder (ticking-clock))
        nanos (long-array 2)
        handler (timings/timed-handler recorder nanos 1 (fn [loc ns-info file] [loc ns-info file]))]
    (is (= [:loc :ns :file] (handler :loc :ns :file))
        "wrapped handler returns what handler returns")
    (handler :loc :ns :file)
    (is (= [0 2] (vec nanos))
        "each call adds one tick to slot 1 only")))

(deftest analyze-file-records-handler-time-per-group
  (let [calls (atom {})
        counting (fn [k result] (fn [_ _ _] (swap! calls update k (fnil inc 0)) result))
        shared (counting :shared nil)
        groups [(fake-group :tokens {:handle-token (counting :token {:usages [:t]})
                                     :handle-vector shared})
                (fake-group :lists {:handle-list [(counting :list {:decls [:l]}) nil]
                                    :handle-vector shared})]
        file (temp-source-file "(ns a.b)\n(defn f [x] (g x :k))\n")
        recorder (timings/recorder (ticking-clock))]
    (try
      (let [untimed (parser/analyze-file file groups)
            _ (reset! calls {})
            timed (parser/analyze-file file groups recorder)
            {:keys [token list shared]} @calls
            [{:keys [handlers parse]
              :as file-times}] @(:files recorder)]
        (is (= untimed timed)
            "recording leaves the result as it was")
        (is (= 1 shared)
            "a handler two groups share runs once per node")
        (is (= {:lists list
                :tokens (+ token shared)}
               handlers)
            "each call ticks once, on the group that first names the handler")
        (is (= 1 parse)
            "parse spans one tick")
        (is (= (inc (* 2 (+ token list shared))) (:walk file-times))
            "walk spans the two reads of every handler call")
        (is (= (str file) (:file file-times))
            "file recorded by its path"))
      (finally
        (io/delete-file file)))))

(deftest run-all-records-discovery-and-analyze-per-group
  (let [recorder (timings/recorder (ticking-clock))
        groups [(fake-group :tokens {:handle-token (constantly nil)})
                (fake-group :lists {})]
        timed (core/run-all ["test-projects/a11y-app/src"] groups recorder)
        {:keys [phases files]
         recorded-groups :groups} (timings/summary recorder)]
    (is (= (core/run-all ["test-projects/a11y-app/src"] groups) timed)
        "recording leaves the results as they were")
    (is (= 1 (:discovery phases))
        "discovery spans one tick")
    (is (= 2 (:analyze phases))
        "analyze spans one tick per group")
    (is (= {:lists 1
            :tokens 1}
           (into {} (map (juxt :group :analyze)) recorded-groups))
        "each group's analyze spans one tick, one with no handlers too")
    (is (seq files)
        "every file recorded")))
