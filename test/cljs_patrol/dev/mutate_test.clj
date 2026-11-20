(ns cljs-patrol.dev.mutate-test
  (:require
   [cljs-patrol.dev.mutate :as mutate]
   [clojure.test :refer [deftest is]]))

(def ^:private source
  (str "(ns app.core)\n"
       "(defn ^:private visible? [x] (when (seq x) true))\n"
       "(defn count-up [n] (comment (when n 1)) (inc n))\n"
       "(comment (when x y))\n"
       "#_(defn gone [] (when x y))\n"
       "(def quoted '(when x y))\n"
       "(def plain {:a 1})\n"))

(defn- swaps-of [mutants] (mapv (juxt :def :row :from :to) mutants))

(deftest mutants-swap-each-operator-of-each-definition
  (is (= [["visible?" 2 "when" "when-not"]
          ["visible?" 2 "seq" "empty?"]
          ["visible?" 2 "true" "false"]
          ["count-up" 3 "inc" "dec"]]
         (swaps-of (mutate/mutants source)))
      "ns form, comment, discarded and quoted code skipped; a private name read through its metadata"))

(deftest changed-lines-read-git-diff-hunks
  (is (= #{3 7 8 9}
         (mutate/changed-lines (str "@@ -3 +3 @@\n-old\n+new\n"
                                    "@@ -10,0 +7,3 @@\n+a\n+b\n+c\n"
                                    "@@ -20,2 +12,0 @@\n-x\n-y\n")))
      "one changed line, three added lines, and a deletion that adds none"))

(deftest summary-counts-each-definition-and-names-the-rest
  (let [summary (mutate/summarize {:defs-in-scope ["a" "b" "c"]
                                   :results [{:def "a"
                                              :outcome :failed}
                                             {:def "a"
                                              :outcome :passed}
                                             {:def "b"
                                              :outcome :unviable}
                                             {:def "b"
                                              :outcome :timeout}]})]
    (is (= [{:def "a"
             :mutants 2
             :killed 1
             :survived 1
             :unviable 0
             :timeout 0}
            {:def "b"
             :mutants 2
             :killed 0
             :survived 0
             :unviable 1
             :timeout 1}]
           (:definitions summary)))
    (is (= ["c"] (:not-mutated summary))
        "definition with no mutant is named")
    (is (== 1/2 (rationalize (:score summary)))
        "unviable and timed-out mutants are not scored")))

(deftest reader-conditionals-load-as-clojure
  (create-ns 'cljs-patrol.dev.mutate-test.sample)
  (mutate/load-text 'cljs-patrol.dev.mutate-test.sample "(def platform #?(:clj :jvm :cljs :js))")
  (is (= :jvm @(ns-resolve 'cljs-patrol.dev.mutate-test.sample 'platform))))
