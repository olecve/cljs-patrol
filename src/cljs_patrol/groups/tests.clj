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

(defn- literal-string-loc? [loc]
  (and loc
       (contains? #{:token :multi-line} (z/tag loc))
       (string? (try (z/sexpr loc) (catch Exception _ nil)))))

(defn- end-row [loc]
  (try (first (second (z/position-span loc))) (catch Exception _ nil)))

(defn- inline-assertion-message
  "Return the message loc of an `(is expr \"…\")` that shares a line with the expression.

  Only a string written out counts. A message named by a symbol is not the shape the
  convention is about, and a message the expression spans several lines above already
  reads as its own line."
  [loc]
  (when (= "is" (some-> loc z/down parser/sym-name))
    (let [message (some-> loc z/down z/rightmost)
          expression (some-> message z/left)]
      (when (and (literal-string-loc? message)
                 expression
                 (not (identical? (z/node expression) (z/node (z/down loc))))
                 (= (parser/position-row message) (end-row expression)))
        message))))

(defn- assertion-finding [message-loc file]
  {:kw (symbol "is")
   :type :assertion-message-inline
   :form (str/replace (str/trim (parser/raw message-loc)) #"\s+" " ")
   :file file
   :row (parser/position-row message-loc)
   :hint "Move the message to its own line, under the expression."})

(defn- leading-article-finding [loc file]
  (when (contains? deftest-heads (some-> loc z/down parser/sym-name))
    (when-let [name-loc (parser/declared-name-loc loc)]
      (when-let [test-name (parser/sym-name name-loc)]
        (when-let [article (leading-article test-name)]
          {:kw (symbol test-name)
           :type :deftest-leading-article
           :article article
           :file file
           :row (parser/position-row name-loc)
           :hint (str "Drop the leading \"" article "-\": "
                      (subs test-name (inc (count article))))})))))

(defn- handle-list [loc _ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (when-let [finding (or (leading-article-finding loc file)
                           (some-> (inline-assertion-message loc) (assertion-finding file)))]
      {:decls [finding]
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:deftest-leading-article (vec (filter #(= :deftest-leading-article (:type %)) declarations))
   :assertion-message-inline (vec (filter #(= :assertion-message-inline (:type %)) declarations))})

(defn- summary-lines* [{:keys [deftest-leading-article assertion-message-inline]}]
  [["Deftest leading article:" (count deftest-leading-article)]
   ["Assertion message inline:" (count assertion-message-inline)]])

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
          "--baseline-write so only new names are reported.")
     :assertion-message-inline
     (str "An assertion's message shares a line with the expression it describes. The two "
          "say different things — one is what ran, the other why the answer matters — and "
          "a reader scanning a test body for either has to read past the other to find it. "
          "Put the message on its own line under the expression, which is how the style "
          "guide's own examples are written. Only a string written out is flagged; a "
          "message named by a symbol is not the shape the convention is about.")})
  (rule->tier [_]
    {:deftest-leading-article :cleanup
     :assertion-message-inline :cleanup})
  (file-extensions [_] #{".cljs" ".cljc"}))

(def group (->TestsGroup))
