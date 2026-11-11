(ns cljs-patrol.timings
  "Where a run spends its time, for --timings.

  A recorder is safe to share between threads: every total sits in an atom, and a file's
  numbers arrive in one swap once the file is done."
  (:require
   [clojure.string :as str])
  (:import
   (java.util.function
    LongSupplier)))

(def ^:private system-clock
  (reify LongSupplier
    (getAsLong [_] (System/nanoTime))))

(def ^:private phase-order
  [:discovery :parse :walk :analyze :report])

(defn recorder
  "Return a new recorder whose clock starts now, reading `clock` (nanoseconds) or the system's."
  ([] (recorder system-clock))
  ([^LongSupplier clock]
   {:clock clock
    :files (atom [])
    :group-analyze (atom {})
    :group-handlers (atom {})
    :open (atom {})
    :phases (atom {})
    :started (.getAsLong clock)}))

(defn now
  "Read the recorder's clock."
  ^long [{:keys [^LongSupplier clock]}]
  (.getAsLong clock))

(defn- add-nanos [totals k nanos]
  (update totals k (fnil + 0) nanos))

(defn record-phase!
  "Add `nanos` to `phase`."
  [recorder phase nanos]
  (swap! (:phases recorder) add-nanos phase nanos))

(defn timed
  "Call `f` and add the time it took to `phase`, returning what `f` returned."
  [recorder phase f]
  (let [start (now recorder)
        result (f)]
    (record-phase! recorder phase (- (now recorder) start))
    result))

(defn start-phase!
  "Open `phase`, which [[summary]] closes."
  [recorder phase]
  (swap! (:open recorder) assoc phase (now recorder)))

(defn timed-handler
  "Wrap parse handler `f` so each call adds the time it took to slot `index` of `nanos`.

  `nanos` belongs to one file, and a file is walked on one thread, so the slot needs no lock."
  [{:keys [^LongSupplier clock]} ^longs nanos index f]
  (let [index (int index)]
    (fn [loc ns-info file]
      (let [start (.getAsLong clock)
            result (f loc ns-info file)]
        (aset nanos index (+ (aget nanos index) (- (.getAsLong clock) start)))
        result))))

(defn record-analyze!
  "Add `nanos` to the time `group-id`'s cross-file analyze step took."
  [recorder group-id nanos]
  (swap! (:group-analyze recorder) add-nanos group-id nanos))

(defn record-file!
  "Add one file's times: `parse` and `walk` in nanoseconds, `handlers` a map of group id to nanoseconds."
  [recorder {:keys [parse walk handlers]
             :as file-times}]
  (swap! (:phases recorder) #(-> %
                                 (add-nanos :parse parse)
                                 (add-nanos :walk walk)))
  (swap! (:group-handlers recorder) #(merge-with + % handlers))
  (swap! (:files recorder) conj file-times))

(defn summary
  "Total what the recorder holds, closing any phase still open at the time of the call.

  {:phases {phase nanos} :total nanos :groups [{:group :handlers :analyze :total}]
   :files [{:file :parse :handlers :total}]}, groups and files slowest first."
  [recorder]
  (let [end (now recorder)
        open (into {} (map (fn [[phase start]] [phase (- end start)])) @(:open recorder))
        handlers @(:group-handlers recorder)
        analyze @(:group-analyze recorder)]
    {:files (->> @(:files recorder)
                 (map (fn [{:keys [file parse walk handlers]}]
                        {:file file
                         :handlers (reduce + 0 (vals handlers))
                         :parse parse
                         :total (+ parse walk)}))
                 (sort-by :total >)
                 vec)
     :groups (->> (into (set (keys handlers)) (keys analyze))
                  (map (fn [group-id]
                         (let [handler-nanos (get handlers group-id 0)
                               analyze-nanos (get analyze group-id 0)]
                           {:analyze analyze-nanos
                            :group group-id
                            :handlers handler-nanos
                            :total (+ handler-nanos analyze-nanos)})))
                  (sort-by (juxt (comp - :total) (comp name :group)))
                  vec)
     :phases (merge-with + @(:phases recorder) open)
     :total (- end (:started recorder))}))

(defn- seconds [nanos]
  (format "%.2fs" (/ nanos 1e9)))

(defn- share [nanos total]
  (if (pos? total)
    (format "%.0f%%" (* 100.0 (/ nanos total)))
    "-"))

(defn- table
  "Lay out `rows` of cells under `header` in columns two spaces apart, the first left-aligned, the rest right-aligned."
  [header rows]
  (let [all-rows (cons header rows)
        widths (apply map (fn [& column] (apply max (map count column))) all-rows)
        line (fn [cells]
               (->> (map (fn [cell width index]
                           (format (str "%" (when (zero? index) "-") width "s") cell))
                         cells widths (range))
                    (str/join "  ")
                    str/trimr
                    (str "  ")))]
    (map line all-rows)))

(defn report-lines
  "Lay out `summary` as the lines --timings prints: the phases recorded, the groups, and the `top-files` slowest files."
  [{:keys [phases total groups files]} top-files]
  (concat
   ["Timings (parse, walk and handlers add up over files):"]
   (table ["phase" "time" "share"]
          (concat (map (fn [phase]
                         (let [nanos (get phases phase)]
                           [(name phase) (seconds nanos) (share nanos total)]))
                       (filter #(contains? phases %) phase-order))
                  [["total" (seconds total) ""]]))
   [""]
   (table ["group" "handlers" "analyze" "total"]
          (map (fn [{:keys [group handlers analyze]
                     :as row}]
                 [(name group) (seconds handlers) (seconds analyze) (seconds (:total row))])
               groups))
   (when (seq files)
     (concat
      [""]
      (table ["slowest files" "total" "parse" "handlers"]
             (map (fn [{:keys [file parse handlers]
                        :as row}]
                    [file (seconds (:total row)) (seconds parse) (seconds handlers)])
                  (take top-files files)))))))
