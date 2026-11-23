(ns cljs-patrol.dev.mutate.recorder
  "Record which files this JVM writes, and when, with Java Flight Recorder.

  Only `cljs-patrol.dev.mutate.guard` loads this namespace, and only when the JDK has the
  `jdk.jfr` module, so a JDK without it can still run the runner."
  (:import
   [java.nio.file Files Path Paths]
   [java.nio.file.attribute FileAttribute]
   [java.time Duration Instant]
   [jdk.jfr Recording]
   [jdk.jfr.consumer RecordedEvent RecordingFile]))

(defn start
  "Start a recording of every file write of this JVM, however short, without stack traces, and return it."
  ^Recording []
  (let [recording (Recording.)]
    (-> recording (.enable "jdk.FileWrite") (.withThreshold Duration/ZERO) .withoutStackTrace)
    (.start recording)
    recording))

(defn- absolute-path
  "Return `path` as an absolute path without `.` or `..` parts, resolved against the directory the JVM started in."
  [^String path]
  (-> (Paths/get path (make-array String 0)) .toAbsolutePath .normalize str))

(defn- write-marker
  "Write one byte to `file`, and return the time halfway through the write, by Instant/now."
  [^java.io.File file]
  ; mutate: skip body: no test can move the JFR clock, so a missing marker only leaves the offset at zero
  (let [before (Instant/now)]
    (with-open [out (java.io.FileOutputStream. file)]
      (.write out 1)) ; mutate: skip 1: any byte is a write
    (let [after (Instant/now)]
      ; mutate: skip 2: the write takes microseconds, so another point in it moves the offset by less than the margin
      (.plusNanos before (quot (.toNanos (Duration/between before after)) 2)))))

(defn writes
  "Return each file write that `recording` holds so far, as {:path … :at …}, in time order.

  The path is absolute, and :at is the Instant the write began, on the clock of Instant/now.
  JFR keeps its own clock, which can drift from that one in a JVM that runs for a long time, so
  a marker write right before the dump measures the offset between the two. A write to the
  console names no file, so it is left out. The recording goes on."
  [^Recording recording]
  (let [file (Files/createTempFile "mutate-writes" ".jfr" (make-array FileAttribute 0))
        marker (.toFile (Files/createTempFile "mutate-marker" ".txt" (make-array FileAttribute 0)))]
    (try
      (let [marked-at (write-marker marker)
            marker-path (absolute-path (str marker))
            _ (.dump recording file)
            events (->> (RecordingFile/readAllEvents file)
                        (keep (fn [^RecordedEvent event]
                                (when-let [path (.getString event "path")]
                                  {:path (absolute-path path)
                                   :at (.getStartTime event)})))
                        distinct)
            ; mutate: skip last: each dump holds one marker write, so its first and last are the same
            offset (if-let [{:keys [at]} (last (filter #(= marker-path (:path %)) events))]
                     (Duration/between marked-at at)
                     Duration/ZERO)]
        (->> events
             (remove #(= marker-path (:path %)))
             (map #(update % :at (fn [^Instant at] (.minus at offset))))
             (sort-by :at)
             vec))
      (finally
        (Files/deleteIfExists ^Path file)
        (.delete marker)))))

(defn close
  "Stop `recording`, and free what it holds."
  [^Recording recording]
  (.close recording))
