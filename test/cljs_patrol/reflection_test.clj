(ns cljs-patrol.reflection-test
  "Guards the native-image build: reflective interop compiles and runs fine on
  the JVM but throws in a GraalVM binary, where reflection metadata is stripped
  unless registered. Without this, only a native run would catch it."
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def ^:private probe-forms
  "Loads every namespace under src/ in a fresh JVM with reflection warnings on.
  Runs out-of-process because cljs-patrol.group defines a protocol: reloading it
  into this JVM would leave the already-built group records failing to satisfy
  the freshly-defined protocol. Fully qualified throughout, and split in two, so
  each form is compiled only after the previous one has loaded what it names."
  ['(require 'clojure.java.io 'clojure.string)
   '(binding [*warn-on-reflection* true]
      (doseq [path (->> (file-seq (clojure.java.io/file "src"))
                        (filter (fn [f] (.isFile ^java.io.File f)))
                        (map (fn [f] (.getPath ^java.io.File f)))
                        (filter (fn [p] (clojure.string/ends-with? p ".clj")))
                        sort)]
        (require (symbol (-> path
                             (clojure.string/replace #"^src/" "")
                             (clojure.string/replace #"\.clj$" "")
                             (clojure.string/replace "_" "-")
                             (clojure.string/replace "/" "."))))))])

(deftest no-reflective-interop-test
  (let [{:keys [exit err]} (sh "java" "-cp" (System/getProperty "java.class.path")
                               "clojure.main" "-e" (str/join " " (map pr-str probe-forms)))
        warnings (->> (str/split-lines (str err))
                      (filter #(str/starts-with? % "Reflection warning")))]
    (is (zero? exit)
        (str "probe JVM failed to load the sources:\n" err))
    (is (empty? warnings)
        (str "Reflective interop found. It works on the JVM but throws in the native binary, "
             "so hint the call sites:\n"
             (str/join "\n" warnings)))))
