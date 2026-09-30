(ns cljs-patrol.groups.tests
  "Rule group: naming conventions on test vars."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private deftest-heads
  "Forms that define a test var.

  `deftest-` is cljs.test's private variant, and a project's own wrapper macro is named
  here only when it is spelled the same way — nothing resolves a macro across namespaces."
  #{"deftest" "deftest-"})

(def ^:private leading-articles
  "Articles that carry nothing at the front of an identifier.

  Only at the front: a test named after the claim it makes is a sentence, and an article
  inside one is grammar rather than noise — `escape-closes-the-modal` reads as intended,
  where `the-modal-closes-on-escape` opens on a word that says nothing about the claim."
  ["a" "an" "the"])

(defn- leading-article
  "Return the article a test name opens with, or nil."
  [test-name]
  (some #(when (str/starts-with? test-name (str % "-")) %) leading-articles))

(defn- handle-list [loc _ns-info file]
  (when (and (not (hiccup/inside-unrendered-form? loc))
             (contains? deftest-heads (some-> loc z/down parser/sym-name)))
    (when-let [name-loc (parser/declared-name-loc loc)]
      (when-let [test-name (parser/sym-name name-loc)]
        (when-let [article (leading-article test-name)]
          {:decls [{:kw (symbol test-name)
                    :type :deftest-leading-article
                    :article article
                    :file file
                    :row (parser/position-row name-loc)
                    :hint (str "Drop the leading \"" article "-\": "
                               (subs test-name (inc (count article))))}]
           :usages []
           :dynamics []})))))

(defn- analyze* [{:keys [declarations]}]
  {:deftest-leading-article (vec (filter #(= :deftest-leading-article (:type %)) declarations))})

(defn- summary-lines* [{:keys [deftest-leading-article]}]
  [["Deftest leading article:" (count deftest-leading-article)]])

(defn- failed?* [_]
  ;; A naming convention blocks nothing at runtime. Opt in with --fail-on when a project
  ;; wants it gated, the way the other cleanup-tier rules are treated.
  false)

(defrecord TestsGroup []
  group/RuleGroup
  (group-id [_] :tests)
  (group-name [_] "Tests")
  (parse-handlers [_] {:handle-list handle-list})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:deftest-leading-article
     (str "A test var opens with \"a-\", \"an-\" or \"the-\". A test named after the claim it "
          "makes is a sentence, and the article at the front of one carries nothing an "
          "identifier needs: the reader is looking for the subject, and the first word is "
          "not it. Drop it — the-status-label-is-rendered becomes "
          "status-label-is-rendered. Only the front is flagged; an article inside the name "
          "is grammar, so escape-closes-the-modal is left alone. Point the tool at the test "
          "directory to use this group, and snapshot an existing codebase with "
          "--baseline-write so only new names are reported.")})
  (rule->tier [_]
    {:deftest-leading-article :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->TestsGroup))
