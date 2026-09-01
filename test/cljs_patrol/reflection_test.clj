(ns cljs-patrol.reflection-test
  "Guards the native-image build against reflective interop.
  Such calls compile and run fine on the JVM but throw in a GraalVM binary,
  where reflection metadata is stripped unless registered, so only a native
  run would otherwise catch them."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]])
  (:import
   (java.io
    File)))

(defn- path->ns-symbol [^String path]
  (-> path
      (str/replace #"^src/" "")
      (str/replace #"\.cljc?$" "")
      (str/replace "_" "-")
      (str/replace "/" ".")
      symbol))

(defn- source-namespaces []
  (->> (file-seq (io/file "src"))
       (filter (fn [^File f] (.isFile f)))
       (map (fn [^File f] (.getPath f)))
       (filter #(or (str/ends-with? % ".clj")
                    (str/ends-with? % ".cljc")))
       (map path->ns-symbol)
       sort
       vec))

(defn- probe-form
  "Builds the code that loads `namespaces` in a fresh JVM with reflection warnings on.
  The probe runs out-of-process because cljs-patrol.group defines a protocol:
  reloading it into this JVM would leave the already-built group records
  failing to satisfy the freshly-defined protocol."
  [namespaces]
  (str "(binding [*warn-on-reflection* true] (doseq [n '" (pr-str namespaces) "] (require n)))"))

(defn- our-reflection-warnings
  "Filters out warnings from dependency code.
  The probe loads dependencies transitively under the same binding, and a
  warning from one of those is not something this build can act on."
  [err]
  (->> (str/split-lines (str err))
       (filter #(and (str/starts-with? % "Reflection warning")
                     (str/includes? % "cljs_patrol/")))))

(deftest no-reflective-interop-test
  (let [namespaces (source-namespaces)
        {:keys [exit err]} (sh "java" "-cp" (System/getProperty "java.class.path")
                               "clojure.main" "-e" (probe-form namespaces))
        warnings (our-reflection-warnings err)]
    (is (seq namespaces)
        "expected to discover source namespaces under src/, otherwise the probe verifies nothing")
    (is (zero? exit)
        (str "probe JVM failed to load the sources:\n" err))
    (is (empty? warnings)
        (str "Reflective interop found. It works on the JVM but throws in the native binary, "
             "so hint the call sites:\n"
             (str/join "\n" warnings)))))
