(ns cljs-patrol.parser-test
  (:require
   [cljs-patrol.disk :as disk]
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.re-frame :as re-frame]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.temp-dir :as temp-dir]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z])
  (:import
   (java.nio.file
    Files
    OpenOption)))

(deftest resolve-kw-test
  (testing "plain keyword"
    (is (= :plain (parser/resolve-kw ":plain" "my.ns" {}))))

  (testing "qualified keyword passes through unchanged"
    (is (= :other.ns/name (parser/resolve-kw ":other.ns/name" "my.ns" {}))))

  (testing "auto-resolved local keyword"
    (is (= :my.ns/local (parser/resolve-kw "::local" "my.ns" {}))))

  (testing "auto-resolved aliased keyword"
    (is (= :full.ns/name (parser/resolve-kw "::alias/name" "my.ns" {"alias" "full.ns"}))))

  (testing "unknown alias returns nil"
    (is (nil? (parser/resolve-kw "::unknown/name" "my.ns" {})))))

(deftest resolve-sym-test
  (testing "unqualified symbol resolves to current ns"
    (is (= :my.ns/fn-name (parser/resolve-sym "fn-name" "my.ns" {}))))

  (testing "aliased symbol resolves via alias map"
    (is (= :full.ns/fn-name (parser/resolve-sym "alias/fn-name" "my.ns" {"alias" "full.ns"}))))

  (testing "unknown alias returns nil"
    (is (nil? (parser/resolve-sym "unknown/fn-name" "my.ns" {})))))

(deftest position-reads-node-metadata
  (let [loc (-> (z/of-string "(ns a)\n\n  (f\n   x)") z/right)]
    (is (= [3 3] (parser/position loc))
        "start of form")
    (is (= [[3 3] [4 6]] (parser/position-span loc))
        "start and end of form")
    (is (= 3 (parser/position-row loc))
        "row of form")))

(deftest position-falls-back-without-metadata
  (let [marker-loc (-> (z/of-string "#?(:cljs 1)") z/down)]
    (is (= [0 1] (parser/position marker-loc))
        "fallback for reader conditional marker")
    (is (nil? (parser/position-span marker-loc))
        "no span for reader conditional marker")
    (is (= 0 (parser/position-row marker-loc))
        "row 0 for reader conditional marker")))

(deftest distinct-by-test
  (testing "removes duplicates by key, last one wins"
    (let [result (parser/distinct-by :id [{:id 1
                                           :v :a}
                                          {:id 2
                                           :v :b}
                                          {:id 1
                                           :v :c}])]
      (is (= 2 (count result)))
      (is (= #{1 2} (set (map :id result))))))

  (testing "empty collection returns empty"
    (is (empty? (parser/distinct-by :id []))))

  (testing "no duplicates returns all items"
    (is (= 3 (count (parser/distinct-by :id [{:id 1} {:id 2} {:id 3}]))))))

(deftest macro-alias-keeps-require-alias-of-same-name
  (let [{:keys [results]} (temp-project/run-group re-frame/group
                                                  {"app/events.cljs" "(ns app.events (:require [re-frame.core :as rf]))
(rf/reg-event-db ::save (fn [db _] db))"
                                                   "app/views.cljs" "(ns app.views
  (:require [app.events :as e] [re-frame.core :as rf])
  (:require-macros [app.macros :as e]))
(defn view [] [:button {:on-click #(rf/dispatch [::e/save])}])"})]
    (is (empty? (:phantom-events results))
        "resolves keyword through require alias")
    (is (empty? (:unused-events results))
        "counts dispatch toward event")))

(deftest macro-aliases-and-refers-are-kept-apart
  (let [ns-info (parser/find-ns-info (z/of-string "(ns app.views
  (:require [app.events :as e :refer [save]] [app.ui :refer-macros [panel]])
  (:require-macros [app.macros :as e :refer [save with-let]]))"))]
    (is (= {:aliases {"e" "app.events"}
            :refers {"save" "app.events"}
            :macro-aliases {"e" "app.macros"}
            :macro-refers {"save" "app.macros"
                           "with-let" "app.macros"
                           "panel" "app.ui"}}
           (select-keys ns-info [:aliases :refers :macro-aliases :macro-refers]))
        "keeps require-macros and refer-macros names out of aliases and refers")))

(defn- test-name-around [loc]
  (some-> (parser/enclosing-test-loc loc) parser/test-name-loc parser/sym-name))

(defn- sub-zipper-and-file-locs
  "Return the same string token reached through a sub-zipper of a test's body and through the file."
  []
  (let [inner-vector (-> (z/of-string "(deftest saves [:div [:b \"x\"]])") z/down z/right z/right)]
    [(-> (z/subzip inner-vector) z/down z/right z/down z/right)
     (-> inner-vector z/down z/right z/down z/right)]))

(deftest enclosing-test-loc-reads-a-sub-zipper-before-the-file
  (binding [parser/*file-memo* (java.util.HashMap.)]
    (let [[in-sub in-file] (sub-zipper-and-file-locs)]
      (is (nil? (parser/enclosing-test-loc in-sub))
          "sub-zipper holds no test")
      (is (= "saves" (test-name-around in-file))
          "file still finds its test after the sub-zipper"))))

(deftest enclosing-test-loc-reads-the-file-before-a-sub-zipper
  (binding [parser/*file-memo* (java.util.HashMap.)]
    (let [[in-sub in-file] (sub-zipper-and-file-locs)]
      (is (= "saves" (test-name-around in-file))
          "file finds its test")
      (is (nil? (parser/enclosing-test-loc in-sub))
          "sub-zipper holds no test though the file's answer is kept"))))

(defn- memo-probe
  "Build a group that records, for every vector, the memo in force and whether a style declaration encloses it."
  [seen]
  (reify group/RuleGroup
    (group-id [_] :memo-probe)
    (group-name [_] "Memo probe")
    (analyze [_ _] {})
    (summary-lines [_ _] [])
    (failed? [_ _] false)
    (suggestions [_] {})
    (docs-anchors [_] {})
    (rule->tier [_] {})
    (file-extensions [_] #{".cljs"})
    (parse-handlers [_]
      {:handle-vector (fn [loc _ file]
                        (swap! seen conj {:file file
                                          :memo parser/*file-memo*
                                          :style-decl? (hiccup/inside-style-decl? loc)})
                        nil)})))

(deftest analyze-file-gives-each-file-own-memo
  (let [dir (io/file (temp-dir/create-dir "cljs-patrol"))
        styles (io/file dir "styles.cljs")
        views (io/file dir "views.cljs")
        seen (atom [])
        probe (memo-probe seen)]
    (try
      (spit styles "(defclass c [] [:a])")
      (spit views "(defn v [] [:a])")
      (parser/analyze-file (disk/path styles) [probe])
      (parser/analyze-file (disk/path views) [probe])
      (let [[in-styles in-views] (map (fn [file] (filter #(= (str file) (:file %)) @seen)) [styles views])]
        (is (= [[true true] [false false]] (map #(map :style-decl? %) [in-styles in-views]))
            "each file answered from own forms")
        (is (every? some? (map :memo @seen))
            "memo bound while file is read")
        (is (every? #(identical? (:memo (first in-styles)) (:memo %)) in-styles)
            "one memo throughout file")
        (is (not (identical? (:memo (first in-styles)) (:memo (first in-views))))
            "fresh memo per file")
        (is (nil? parser/*file-memo*)
            "memo unbound after file"))
      (finally
        (temp-dir/delete-dir! dir)))))

(defn- file-probe
  "Build a group whose file handler calls `on-file` with the file's path and declares the file."
  [on-file]
  (reify group/RuleGroup
    (group-id [_] :file-probe)
    (group-name [_] "File probe")
    (analyze [_ _] {})
    (summary-lines [_ _] [])
    (failed? [_ _] false)
    (suggestions [_] {})
    (docs-anchors [_] {})
    (rule->tier [_] {})
    (file-extensions [_] #{".cljs"})
    (parse-handlers [_]
      {:handle-file (fn [_ _ file]
                      (on-file file)
                      {:decls [{:file file
                                :memo parser/*file-memo*
                                :thread (Thread/currentThread)}]
                       :dynamics []
                       :usages []})})))

(defn- text-probe
  "Build a group whose file handler adds the text that the parser read to `texts`."
  [texts]
  (reify group/RuleGroup
    (group-id [_] :text-probe)
    (group-name [_] "Text probe")
    (analyze [_ _] {})
    (summary-lines [_ _] [])
    (failed? [_ _] false)
    (suggestions [_] {})
    (docs-anchors [_] {})
    (rule->tier [_] {})
    (file-extensions [_] #{".cljs"})
    (parse-handlers [_]
      {:handle-file (fn [zloc _ _]
                      (swap! texts conj (z/root-string zloc))
                      nil)})))

(deftest analyze-file-reads-bytes-that-are-not-utf-8-as-z-of-file-does
  (let [dir (temp-dir/create-dir "cljs-patrol")
        file (disk/path dir "a.cljs")
        texts (atom [])]
    (try
      (Files/write file
                   (byte-array (concat (.getBytes "(ns a)\r\n(def x \"" "UTF-8") [0xC3 0x28] (.getBytes "\")\n" "UTF-8")))
                   (make-array OpenOption 0))
      (parser/analyze-file file [(text-probe texts)])
      (is (= [(z/root-string (z/of-file (str file)))] @texts)
          "same text as z/of-file")
      (is (= ["(ns a)\n(def x \"\uFFFD(\")\n"] @texts)
          "U+FFFD for byte C3, and newlines normalized")
      (finally
        (temp-dir/delete-dir! dir)))))

(deftest analyze-project-prints-source-folder-as-typed
  (let [dir (temp-dir/create-dir "cljs-patrol")
        errors (java.io.StringWriter.)]
    (try
      (spit (str dir "/a.cljs") "(ns a)")
      (binding [*err* errors]
        (parser/analyze-project {:path (disk/path dir)
                                 :text (str dir "//")}
                                [(file-probe (constantly nil))]))
      (is (= (str "Analyzing 1 files under " dir "// ...\n") (str errors))
          "typed text kept, though Path drops trailing slashes")
      (finally
        (temp-dir/delete-dir! dir)))))

(defn- with-project
  "Write `sources`, a map of file name to source, to a temporary directory and call `f` with it and its files in walk order."
  [sources f]
  (let [dir (io/file (temp-dir/create-dir "cljs-patrol"))]
    (try
      (doseq [[file-name source] sources]
        (spit (io/file dir file-name) source))
      (f (str dir) (fs/list-source-files (str dir) #{".cljs"}))
      (finally
        (temp-dir/delete-dir! dir)))))

(def ^:private four-files
  {"a.cljs" "(ns a)"
   "b.cljs" "(ns b)"
   "c.cljs" "(ns c)"
   "d.cljs" "(ns d)"})

(defn- analyze-quietly [dir groups jobs]
  (binding [*err* (java.io.StringWriter.)]
    (doall (:declarations (parser/analyze-project (disk/source dir) groups nil jobs)))))

(deftest parallel-results-keep-file-order
  (with-project four-files
    (fn [dir files]
      (let [finished (atom [])
            probe (file-probe (fn [file]
                                (when (= file (first files))
                                  (Thread/sleep 300))
                                (swap! finished conj file)))
            declarations (analyze-quietly dir [probe] 4)]
        (is (not= (first files) (first @finished))
            "first file finishes after others")
        (is (= files (map :file declarations))
            "results in file order")))))

(deftest parallel-run-submits-every-file-before-awaiting-first
  (let [file-count 40]
    (with-project (into {} (map #(vector (format "f%02d.cljs" %) (str "(ns f" % ")"))) (range file-count))
      (fn [dir files]
        (let [others-done (java.util.concurrent.CountDownLatch. (dec file-count))
              others-awaited (promise)
              probe (file-probe (fn [file]
                                  (if (= file (first files))
                                    (deliver others-awaited (.await others-done 5 java.util.concurrent.TimeUnit/SECONDS))
                                    (.countDown others-done))))]
          (analyze-quietly dir [probe] 4)
          (is (true? @others-awaited)
              "later files read while first file waits"))))))

(deftest empty-project-gives-no-declarations
  (with-project {}
    (fn [dir _]
      (is (empty? (analyze-quietly dir [(file-probe (constantly nil))] 4))))))

(deftest parallel-warnings-keep-file-order
  (with-project {"a.cljs" "(ns a"
                 "b.cljs" "(ns b"
                 "c.cljs" "(ns c"}
    (fn [dir files]
      (spit (first files) (str (apply str (repeat 20000 "(defn f [x] [:div {:a 1} x])\n")) "(ns a"))
      (let [errors (java.io.StringWriter.)]
        (binding [*err* errors]
          (doall (:declarations (parser/analyze-project (disk/source dir) [(file-probe (constantly nil))] nil 3))))
        (is (= files
               (->> (str/split-lines (str errors))
                    (keep #(second (re-matches #"WARN: could not parse (\S+) :.*" %)))))
            "warnings in file order, on caller's stderr")))))

(deftest parallel-run-rethrows-first-failing-file
  (with-project four-files
    (fn [dir files]
      (let [probe (file-probe (fn [file]
                                (condp = file
                                  (first files) (do (Thread/sleep 300)
                                                    (throw (ex-info "first failed" {:file file})))
                                  (second files) (throw (ex-info "second failed" {:file file}))
                                  nil)))
            thrown (try
                     (analyze-quietly dir [probe] 4)
                     nil
                     (catch Exception e e))]
        (is (instance? clojure.lang.ExceptionInfo thrown)
            "exception thrown by handler, not wrapped")
        (is (= {:file (first files)} (ex-data thrown))
            "first failing file in file order")))))

(defn- live-workers []
  (filter #(= "cljs-patrol-worker" (.getName ^Thread %)) (keys (Thread/getAllStackTraces))))

(defn- await-no-workers []
  (loop [attempts 50]
    (when (and (seq (live-workers)) (pos? attempts))
      (Thread/sleep 20)
      (recur (dec attempts))))
  (live-workers))

(deftest parallel-run-shuts-pool-down
  (with-project four-files
    (fn [dir files]
      (let [daemon (atom [])
            probe (file-probe (fn [file]
                                (swap! daemon conj (.isDaemon (Thread/currentThread)))
                                (when (= file (last files))
                                  (throw (ex-info "failed" {})))))]
        (is (thrown? clojure.lang.ExceptionInfo (analyze-quietly dir [probe] 4)))
        (is (every? true? @daemon)
            "workers are daemon threads")
        (is (empty? (await-no-workers))
            "no worker left after failure")
        (analyze-quietly dir [(file-probe (constantly nil))] 4)
        (is (empty? (await-no-workers))
            "no worker left after success")))))

(defn- count-identities [values]
  (let [identities (java.util.IdentityHashMap.)]
    (doseq [value values]
      (.put identities value true))
    (.size identities)))

(deftest parallel-run-gives-each-file-own-memo
  (with-project four-files
    (fn [dir _]
      (let [caller-memo (java.util.HashMap.)
            declarations (binding [parser/*file-memo* caller-memo]
                           (analyze-quietly dir [(file-probe (constantly nil))] 4))
            memos (map :memo declarations)]
        (is (every? some? memos)
            "memo bound in every task")
        (is (not-any? #(identical? caller-memo %) memos)
            "caller's memo not conveyed")
        (is (= 4 (count-identities memos))
            "fresh memo per file")))))

(deftest one-job-reads-files-on-one-worker
  (with-project four-files
    (fn [dir files]
      (let [caller (Thread/currentThread)
            sequential (analyze-quietly dir [(file-probe (constantly nil))] 1)
            parallel (analyze-quietly dir [(file-probe (constantly nil))] 4)]
        (is (= files (map :file sequential))
            "results in file order")
        (is (= ["cljs-patrol-worker"] (distinct (map #(.getName ^Thread (:thread %)) sequential)))
            "one job reads on worker")
        (is (= 1 (->> sequential (map :thread) distinct count))
            "one job reads on single thread")
        (is (not-any? #(identical? caller (:thread %)) parallel)
            "four jobs leave caller")))))

(defn- call-on-small-stack
  "Call `f` on a thread with a 512 KiB stack, the size a native binary gives a thread it starts, and give back what it returns or throws."
  [f]
  (let [outcome (promise)]
    (doto (Thread. nil
                   #(deliver outcome (try (f) (catch Throwable thrown thrown)))
                   "small-stack-caller"
                   (long (* 512 1024)))
      (.start))
    @outcome))

(def ^:private nesting-depth 2000)

(deftest deep-nesting-reads-alike-at-any-jobs
  (with-project {"deep.cljs" (str "(ns deep (:require [re-frame.core :as rf]))\n"
                                  "(rf/reg-sub ::items (fn [_] "
                                  (str/join (repeat nesting-depth "["))
                                  (str/join (repeat nesting-depth "]"))
                                  "))\n")}
    (fn [dir _]
      (let [declarations-with #(call-on-small-stack (fn [] (analyze-quietly dir [re-frame/group] %)))
            sequential (declarations-with 1)
            parallel (declarations-with 4)]
        (is (sequential? sequential)
            "one job reads deep file without overflow")
        (is (= [:deep/items] (map :kw sequential))
            "subscription found past deep form")
        (is (= sequential parallel)
            "one job and four jobs agree")))))

(deftest file-state-test
  (binding [parser/*file-memo* (java.util.HashMap.)]
    (let [state (parser/file-state ::probe 0)]
      (swap! state inc)
      (is (identical? state (parser/file-state ::probe 0))
          "same atom for same key within file")
      (is (= 1 @(parser/file-state ::probe 0))
          "initial value used once")
      (is (not (identical? state (parser/file-state ::other 0)))
          "own atom per key")
      (binding [parser/*file-memo* (java.util.HashMap.)]
        (is (= 0 @(parser/file-state ::probe 0))
            "next file starts afresh"))))
  (is (not (identical? (parser/file-state ::probe 0) (parser/file-state ::probe 0)))
      "fresh atom per call outside file"))
