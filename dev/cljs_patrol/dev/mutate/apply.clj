(ns cljs-patrol.dev.mutate.apply
  "Write one mutant into its file for --apply, and put the file back for --restore."
  (:require
   [cljs-patrol.dev.mutate.error :as error]
   [cljs-patrol.dev.mutate.forms :as forms]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn apply-command
  "Return the command that writes `mutant` of `target` into its file, as the reports show it, quoted for a shell."
  [target {:keys [row col from]}]
  (str "clojure -M:test:mutate " target " --apply '" row ":" col ":" from "'"))

(defn relative-path
  "Return the path of `file` from the working directory, or the whole path when it lies outside it."
  [file]
  (let [path (.toPath (.getCanonicalFile (io/file file)))
        cwd (.toPath (.getCanonicalFile (io/file ".")))]
    (str (if (.startsWith path cwd) (.relativize cwd path) path))))

(def applied-file
  "Where --apply keeps the text it replaced, until --restore puts it back: outside target/, which a build deletes."
  ".mutate-applied.edn")

(defn- offset
  "Return the index in a text of `lines` where `row` and `col` start."
  [lines row col]
  (+ (->> lines (take (dec row)) (map (comp inc count)) (reduce +)) (dec col)))

(defn applied-mutant
  "Return what --apply kept in `kept`, {:file … :original … :applied …}, or nil when no mutant is applied.

  A kept file that holds anything else stops the run."
  [kept]
  (when (.exists (io/file kept))
    (let [value (try (edn/read-string (slurp kept)) (catch Exception _ nil))]
      (if (and (map? value) (every? #(string? (get value %)) [:file :original :applied]))
        value
        (throw (error/setup-error (str kept " does not hold what --apply keeps. If no mutant is applied, delete it.")))))))

(defn write-whole
  "Write `text` to `file` through a temporary file beside it, so that the file holds its old text or all of the new."
  [file text]
  (let [path (.toPath (.getCanonicalFile (io/file file)))
        temp (java.nio.file.Files/createTempFile (.getParent path) ".mutate" ".tmp"
                                                 (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile temp) text)
      (java.nio.file.Files/move temp path (into-array java.nio.file.CopyOption
                                                      [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                                                       java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
      (finally
        (java.nio.file.Files/deleteIfExists temp)))))

(defn apply-mutant
  "Write the mutant at `position`, written ROW:COL:OPERATOR, into `file`, keep its text in `kept`, and return a message.

  Only the one token changes, and only when that operator is found at that place, so a position
  from an older report cannot write another mutant. The file is replaced whole, so a failed
  write leaves it as it was. A second mutant cannot be applied before --restore puts the first
  one back."
  [file position kept]
  (when-let [{applied-file :file} (applied-mutant kept)]
    (throw (error/setup-error (str "A mutant is already applied to " (relative-path applied-file)
                                   ". Put the code back with --restore first."))))
  (let [[_ row col operator] (re-find #"^(\d+):(\d+):(\S+)$" (str position))
        _ (when-not row
            (throw (error/setup-error (str "--apply takes ROW:COL:OPERATOR, such as '490:43:remove', not " position "."))))
        row (parse-long row)
        col (parse-long col)
        source (slurp file)
        on-row (->> source forms/top-level-forms forms/mutants (filter #(= row (:row %))))
        {:keys [from to]
         function :def
         :as mutant} (first (filter #(= col (:col %)) on-row)) ; mutate: skip first: one mutant per line and column
        path (relative-path file)]
    (when-not mutant
      (throw (error/setup-error (str "No mutant at " row ":" col "."
                                     (when (seq on-row)
                                       (str " Line " row " has "
                                            (str/join ", " (map #(str row ":" (:col %) " " (:from %)) on-row))
                                            "."))
                                     " Run the report again for the current positions."))))
    (when-not (= operator from)
      (throw (error/setup-error (str "At " row ":" col " the operator is now " from ", not " operator "."
                                     " Run the report again for the current positions."))))
    (let [start (offset (str/split source #"\n" -1) row col)
          end (+ start (count from))]
      ; mutate: skip <=: a form ends after its operator, so end never passes the last character
      (when-not (and (<= end (count source)) (= from (subs source start end)))
        (throw (error/setup-error (str "The text at " row ":" col " of " path " is not " from ", so nothing was written."
                                       " The file may break lines in a way the runner does not count."))))
      (spit kept (pr-str {:file (.getCanonicalPath (io/file file))
                          :original source
                          :applied (str (subs source 0 start) (pr-str to) (subs source end))}))
      (try
        (write-whole file (:applied (applied-mutant kept)))
        (catch Exception e
          (io/delete-file kept)
          (throw (error/setup-error (str "Could not write " path ", which is left as it was: " (ex-message e)))))))
    (str "Applied " row ":" col "  " from " -> " (pr-str to) "  in " function " to " path ".\n"
         "Run the tests and watch the new test fail, then put the code back with:\n"
         "  clojure -M:test:mutate --restore")))

(defn restore-mutant
  "Put back the text that --apply replaced, as kept in `kept`, and return a message.

  A file already back as it was only loses the kept text. A file that changed in any other way,
  or is gone, is left alone, so an edit made after the apply is never lost."
  [kept]
  (let [{:keys [file original applied]
         :as kept-text} (applied-mutant kept)
        way-out (str "The text from before the apply is in " kept "; once the file is as you want it, delete " kept ".")]
    (when-not kept-text
      (throw (error/setup-error "No mutant is applied, so there is nothing to restore.")))
    (when-not (.exists (io/file file))
      (throw (error/setup-error (str (relative-path file) " is gone, so --restore cannot put it back. " way-out))))
    (let [current (slurp file)]
      (cond
        (= applied current) (do (write-whole file original)
                                (io/delete-file kept)
                                (str "Restored " (relative-path file) "."))
        (= original current) (do (io/delete-file kept)
                                 (str (relative-path file) " was already back as before the apply."))
        :else (throw (error/setup-error (str (relative-path file) " changed since the mutant was applied, so --restore leaves"
                                             " it alone. " way-out)))))))
