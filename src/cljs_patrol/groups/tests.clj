(ns cljs-patrol.groups.tests
  "Rule group: conventions of test code."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.heads :as heads]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   (java.util IdentityHashMap)))

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
  (-> loc parser/position-span second first))

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

(defn- assertion-finding
  "Build the finding for an assertion whose message shares the expression's line.

  `:form` is the whole assertion, not the message alone. Two assertions in one file
  often carry the same message over different expressions — `(is (.-checked checkbox-1)
  \"…\")` beside `(is (.-checked checkbox-3) \"…\")` — and the baseline keys on the form,
  so a message-only form would make them one entry: fix either and the other stays
  suppressed."
  [is-loc message-loc file]
  {:kw (symbol "is")
   :type :assertion-message-inline
   :form (parser/normalize-form (parser/raw is-loc))
   :file file
   :row (parser/position-row message-loc)
   :hint "Move the message to its own line, under the expression."})

(def ^:private conditional-heads
  "Forms whose value depends on a test evaluated while the test body runs."
  #{"if" "if-not" "when" "when-not" "cond" "condp" "case" "if-let" "when-let" "if-some" "when-some"})

(def ^:private assertion-heads #{"is"})

(defn- inside-function-literal?
  "True when a conditional sits in a function handed to something else, up to `stop`.

  `(some #(when (= 1 (:id %)) %) xs)` branches per element of a collection, which is a
  predicate rather than the test choosing what to assert."
  [loc stop]
  (loop [current (z/up loc)]
    (cond
      (or (nil? current) (identical? (z/node current) (z/node stop))) false
      (= :fn (z/tag current)) true
      (and (= :list (z/tag current))
           (contains? #{"fn" "fn*"} (parser/sym-name (z/down current)))) true
      :else (recur (z/up current)))))

(defn- holds-assertion? [loc]
  (boolean (some #(and (= :list (z/tag %))
                       (contains? assertion-heads (parser/sym-name (z/down %))))
                 (parser/descendant-locs loc))))

(defn- conditional-in-assertion
  "Return the conditional an assertion computes its expectation with, or nil."
  [loc]
  (when (contains? assertion-heads (some-> loc z/down parser/sym-name))
    (->> loc
         parser/descendant-locs
         (filter #(and (= :list (z/tag %))
                       (contains? conditional-heads (parser/sym-name (z/down %)))
                       (not (inside-function-literal? % loc))))
         first)))

(def ^:private branch-heads #{"if" "if-not" "cond" "condp" "case"})

(defn- conditional-around-assertion
  "Return `:picks` or `:skips` when a conditional decides what gets asserted.

  `:picks` for a form with arms, where the test chooses between assertions rather than
  stating one. `:skips` for a guard, which is the worse of the two: when it does not
  hold, the test runs no assertion and passes without having checked anything."
  [loc]
  (when-let [head (some-> loc z/down parser/sym-name)]
    (when (and (contains? conditional-heads head) (holds-assertion? loc))
      (if (contains? branch-heads head) :picks :skips))))

(def ^:private conditional-hints
  {:computes "The expected value is computed here. State it instead, one case per row of a table."
   :picks "The test chooses between assertions. Pair each input with its expected value instead."
   :skips "This guard can skip the assertion, and the test then passes having checked nothing."})

(defn- conditional-finding [loc kind head file]
  {:kw (symbol head)
   :type :conditional-assertion
   :form (parser/normalize-form (parser/raw loc))
   :file file
   :row (parser/position-row loc)
   :hint (get conditional-hints kind)})

(defn- enclosing-list-head
  "Return the head symbol's raw text for the nearest enclosing list satisfying pred."
  [loc pred]
  (loop [current (z/up loc)]
    (cond
      (nil? current) nil
      (and (= :list (z/tag current))
           (let [head (some-> current z/down parser/raw)]
             (and head (pred head)))) (some-> current z/down parser/raw)
      :else (recur (z/up current)))))

(defn- path-segments [path]
  (remove str/blank? (str/split (str path) #"[/\\\\]")))

(defn- under-path?
  "True when `file` lies under `configured`, matched a whole segment at a time.

  Segment-wise so `test` does not match `src/cljs/latest/…`. A configured path may be
  written as one segment or several — `test` and `ui/test` both work — and matches
  wherever that run of segments appears, since the tool is pointed at whatever directory
  the caller chooses and the file paths it reports are relative to that."
  [file configured]
  (let [wanted (path-segments configured)
        actual (path-segments file)]
    (and (seq wanted)
         (boolean (some #(= wanted (take (count wanted) %))
                        (take-while seq (iterate rest actual)))))))

(defn- test-file?
  "True when the file holds test code.

  Configured paths win when a project sets them: the directory a test lives in is a fact,
  where the namespace's name is a convention. With none set the convention is the
  fallback, so the group works pointed at a test directory without any configuration —
  and it has to answer one way or the other, since the group runs by default and the
  rules anchored on this would otherwise report production code."
  [paths {:keys [ns-name]} file]
  (if (seq paths)
    (boolean (some #(under-path? file %) paths))
    (boolean (and ns-name (str/ends-with? ns-name "-test")))))

(defn- inside-written-out-quote?
  "True when a `(quote …)` list encloses loc.

  `hiccup/unrendered-form?` reads the reader spellings, `'x` and `#_x`, which are their
  own node types. Written out, a quote is an ordinary list and nothing marks it."
  [loc]
  (some? (enclosing-list-head loc #{"quote"})))

(defn- up-through-meta [loc]
  (loop [current (some-> loc z/up)]
    (if (= :meta (some-> current z/tag)) (recur (z/up current)) current)))

(defn- var-quote-loc
  "Return the var quote `loc` is part of, written either way, or nil.

  `#'x` is a `:var` node around a token; `(var x)` is an ordinary list. Only those two
  node types reach a handler — a `:deref` node reaches none — so the deref is found by
  looking up from the var quote rather than down from the `@`."
  [loc]
  (cond
    (and (= :list (z/tag loc)) (= "var" (some-> loc z/down parser/raw))) loc

    ;; Only from the token the var quotes. `#'^:tag x` holds two tokens — the metadata's
    ;; and the value's — and both climb to the same `:var`, so reporting from either
    ;; reports the one deref twice.
    (let [var-loc (up-through-meta loc)]
      (and (= :var (some-> var-loc z/tag))
           (identical? (z/node loc) (some-> var-loc z/down heads/unwrap-meta z/node))))
    (up-through-meta loc)))

(defn- deref-around
  "Return the deref reading this var quote, or nil.

  `@` is a `:deref` node and `(deref …)` a list. The head is compared raw, so
  `helpers/deref` — someone else's function whose name merely ends the same way — is not
  this one."
  [var-loc]
  (let [parent (up-through-meta var-loc)]
    (cond
      (= :deref (some-> parent z/tag)) parent
      (and (= :list (some-> parent z/tag))
           (= "deref" (some-> parent z/down parser/raw))
           (not (identical? (z/node var-loc) (z/node (z/down parent))))) parent)))

(defn- var-deref-finding
  "Build the finding for a var quote read through a deref in a test namespace.

  `:form` is the whole deref and `:test` the definition it sits in. Two reads inside one
  definition still collapse to a single baseline identity, which no key that survives
  reformatting can help."
  [loc test-name file]
  {:kw (symbol "deref")
   :type :var-deref-in-test
   :form (parser/normalize-form (parser/raw loc))
   :test test-name
   :file file
   :row (parser/position-row loc)
   :hint "Call it through the public entry point that uses it, or move it somewhere it can be public."})

(defn- var-deref-in-test
  "Return the finding for a var quote read through a deref in a test namespace, or nil."
  [paths loc ns-info file]
  (when (test-file? paths ns-info file)
    (when-let [var-loc (var-quote-loc loc)]
      (when-let [deref-loc (deref-around var-loc)]
        (when (and (not (hiccup/inside-unrendered-form? deref-loc))
                   (not (inside-written-out-quote? deref-loc)))
          (var-deref-finding deref-loc (parser/enclosing-definition-name deref-loc) file))))))

(def ^:private js-tag "js")

(defn- js-tag-token? [loc]
  (and (= :token (z/tag loc)) (= js-tag (parser/raw loc))))

(defn- leftmost-child? [loc parent]
  (and parent (identical? (z/node loc) (some-> parent z/down z/node))))

(defn- js-type-hint
  "Return the metadata node a bare `^js` hint is written on, or nil.

  Both spellings are the same hint: `^js x` puts the tag symbol straight into the
  metadata node, `^{:tag js} x` puts it under `:tag` in a map there. Only the bare symbol
  counts — `^js/Foo` names a type and is a different thing, and `^clj`, `^boolean` and
  `^:private` are other tags entirely.

  A `:meta` node reaches no handler and neither does a `:map`, so the hint is found from
  the `js` symbol inside it and read upward."
  [loc]
  (when (js-tag-token? loc)
    (let [parent (z/up loc)]
      (cond
        (and (= :meta (some-> parent z/tag)) (leftmost-child? loc parent))
        parent

        (and (= :map (some-> parent z/tag))
             (= :meta (some-> parent z/up z/tag))
             (leftmost-child? parent (z/up parent))
             (identical? (z/node loc) (some-> (hiccup/literal-map parent) :tag z/node)))
        (z/up parent)))))

(def ^:private let-heads
  "Forms whose bindings vector pairs a name with the value it takes, and the name with its tag."
  #{"let" "loop" "when-let" "if-let" "when-some" "if-some"})

(def ^:private loop-heads
  "Forms whose bindings vector binds names to elements of a collection."
  #{"doseq" "for"})

(def ^:private params-heads
  "Forms whose first vector, or the first vector of each arity, is a params vector."
  #{"fn" "fn*" "defn" "defn-"})

(def ^:private method-heads
  "Forms whose method lists open with a name and a params vector."
  #{"reify" "deftype" "defrecord" "extend-type" "extend-protocol" "specify" "specify!"})

(def ^:private field-heads
  "Forms whose second argument is a vector of fields."
  #{"deftype" "defrecord"})

(def ^:private passing-heads
  "Names of forms outside Clojure's own core that can hand a value on, bind it, or read it.

  A value written as one of their arguments may reach an interop call, so a hint on it is
  left alone. A head counts by its name, qualified or not. Core macros and special forms
  count through [[core-macro?]], and every name that starts with `def` or `with-` counts too,
  so `s/defn` and `m/with-canvas` do."
  #{"this-as" "specify" "specify!" "simple-benchmark" "exists?" "implements?" "satisfies?" "goog-define"
    "unsafe-cast" "are" "async" "testing" "go" "go-loop" "alt!"})

(def ^:private core-macro-names
  "Names of Clojure's own core macros, read once from its public vars."
  (into #{}
        (keep (fn [[sym core-var]] (when (:macro (meta core-var)) (name sym))))
        (ns-publics 'clojure.core)))

(defn- core-macro?
  "True if `core-name` names a core macro or a special form."
  [core-name]
  (or (contains? core-macro-names core-name)
      (special-symbol? (symbol core-name))))

(defn- macro-head?
  "True if the ns form names the head `head-text` as a macro.

  That is a `:refer-macros` or `:require-macros` refer, or an alias of a macro namespace."
  [head-text ns-info]
  (let [[alias head-name] (if (str/includes? head-text "/") (str/split head-text #"/" 2) [nil head-text])]
    (if alias
      (contains? (:macro-aliases ns-info) alias)
      (contains? (:macro-refers ns-info) head-name))))

(defn- passing-name? [head-name]
  (or (contains? passing-heads head-name)
      (str/starts-with? head-name "def")
      (str/starts-with? head-name "with-")))

(defn- letfn-spec?
  "True if the list at `loc` is a function spec in the bindings of `letfn`, not a call."
  [loc]
  (let [bindings (z/up loc)]
    (and (= :vector (some-> bindings z/tag))
         (= "letfn" (some-> bindings z/up z/down parser/sym-name)))))

(defn- resolve-head-kind [loc ns-info]
  (let [head (z/down loc)
        head-name (parser/sym-name head)
        core-name (some-> (heads/resolve-head loc ns-info) (as-> s (when (= "cljs.core" (namespace s)) (name s))))
        bare-name (when (and head-name (= head-name (parser/raw head))) head-name)
        known-name (or core-name bare-name)]
    (cond
      (letfn-spec? loc) [:passing nil]
      (parser/kw-node? head) [:other nil]
      (nil? head-name) [:passing nil]
      (str/starts-with? head-name ".") [:interop nil]
      (or (passing-name? head-name)
          (core-macro? head-name)
          (macro-head? (parser/raw head) ns-info)) [:passing known-name]
      core-name [:core-fn core-name]
      :else [:other nil])))

(defn- head-info
  "Return `[kind name]` for what the list at `loc` opens with, or nil when loc is no list.

  The kind is `:interop`, `:passing`, `:core-fn` or `:other`, and the name is the core name
  the head reads as, when it has one. A bare name the resolver cannot place is read as
  core's, so a form under `:refer :all` is still seen: an unknown head only makes the rule
  leave a hint alone, never flag it."
  [loc ns-info]
  (when (#{:list :fn} (some-> loc z/tag))
    (let [^IdentityHashMap memo @(parser/file-state ::head-info (IdentityHashMap.))]
      (or (.get memo (z/node loc))
          (let [info (resolve-head-kind loc ns-info)]
            (.put memo (z/node loc) info)
            info)))))

(defn- head-kind [loc ns-info]
  (first (head-info loc ns-info)))

(defn- core-head
  "Return the name the list at `loc` opens with when it reads as a core form, or nil.

  That is the core name a head resolves to, or the bare written name of an unqualified head the resolver cannot place,
  read as core's on purpose."
  [loc ns-info]
  (second (head-info loc ns-info)))

(declare other-platforms)

(defn- read-as-nothing?
  "True if shadow-cljs reads the form at `loc` as nothing.

  That is `#_`, or a reader conditional whose every branch is for another platform."
  [loc]
  (or (= :uneval (z/tag loc))
      (and (= :reader-macro (z/tag loc))
           (= "?" (some-> loc z/down parser/raw))
           (every? #(contains? other-platforms (parser/raw %))
                   (take-nth 2 (some-> loc z/down z/right parser/child-locs))))))

(defn- code-index
  "Return the position of loc among its parent's children, as shadow-cljs reads them."
  [loc]
  (->> (iterate z/left (z/left loc))
       (take-while some?)
       (remove read-as-nothing?)
       count))

(defn- reader-conditional-branch
  "Return the platform keyword of the reader conditional branch `loc` is, or nil."
  [loc]
  (let [branches (z/up loc)
        reader (some-> branches z/up)]
    (when (and (= :reader-macro (some-> reader z/tag))
               (#{"?" "?@"} (some-> reader z/down parser/raw)))
      (some-> loc z/left parser/raw))))

(def ^:private other-platforms
  "Reader conditional features of platforms other than ClojureScript.

  A branch for one of them never compiles under shadow-cljs. Any other feature may: the
  reader takes the first one that matches, `:default` always does, and a build can add its
  own `:reader-features`."
  #{":clj" ":cljr" ":clr" ":bb" ":lpy" ":cljd" ":jank"})

(defn- compiled-code?
  "True unless loc sits in code that shadow-cljs never compiles.

  That is `#_`, a quoted form or a syntax-quoted one outside `~`, a `(comment …)` block, and
  a reader conditional branch for another platform."
  [loc ns-info]
  (let [^IdentityHashMap memo @(parser/file-state ::compiled-code (IdentityHashMap.))]
    (if (.containsKey memo (z/node loc))
      (.get memo (z/node loc))
      (let [answer (loop [current loc
                          unquoted? false]
                     (let [parent (z/up current)
                           platform (reader-conditional-branch current)]
                       (cond
                         (nil? parent) true
                         (contains? other-platforms platform) false
                         (#{:uneval :quote} (z/tag parent)) false
                         (#{:unquote :unquote-splicing} (z/tag parent)) (recur parent true)
                         (= :syntax-quote (z/tag parent)) (when unquoted? (recur parent false))
                         (#{"comment" "quote"} (core-head parent ns-info)) false
                         :else (recur parent unquoted?))))]
        (.put memo (z/node loc) answer)
        answer))))

(defn- outermost-meta
  "Return the outermost metadata node wrapped around loc, or loc itself.

  `^:private ^js x` nests one metadata node inside the other, so the place a hinted form
  sits in its parent is the place of the outermost one."
  [loc]
  (loop [current loc]
    (let [parent (z/up current)]
      (if (and (= :meta (some-> parent z/tag))
               (identical? (z/node current) (some-> parent z/down z/rightmost z/node)))
        (recur parent)
        current))))

(defn- tagged-meta?
  "True if the metadata node `meta-loc` gives its form a tag: `^js`, `^js/Foo`, `^{:tag …}`."
  [meta-loc]
  (let [tag-loc (z/down meta-loc)]
    (case (z/tag tag-loc)
      :token (boolean (re-matches #"js(/.+)?" (parser/raw tag-loc)))
      :map (let [tag (some-> (hiccup/literal-map tag-loc) :tag parser/raw)]
             (boolean (and tag (not= "nil" tag))))
      false)))

(defn- own-tag?
  "True if a metadata node around the occurrence at `outer` gives it a tag of its own."
  [outer]
  (boolean (some tagged-meta? (take-while #(= :meta (some-> % z/tag))
                                          (iterate #(some-> % z/down z/rightmost) outer)))))

(defn- params-vector?
  "True if the vector `outer` sits where a params or fields vector of a binding form goes."
  [outer ns-info]
  (let [parent (z/up outer)
        head (core-head parent ns-info)
        grand-head (core-head (some-> parent z/up) ns-info)
        first-vector? #(identical? (z/node outer)
                                   (some->> % parser/written-child-locs
                                            (filter (fn [child] (= :vector (z/tag (heads/unwrap-meta child)))))
                                            first
                                            z/node))]
    (boolean
     (or (and (contains? params-heads head) (first-vector? parent))
         (and (contains? field-heads head) (= 2 (code-index outer)))
         (and (= :list (some-> parent z/tag))
              (or (and (contains? params-heads grand-head) (zero? (code-index outer)))
                  (and (contains? method-heads grand-head) (first-vector? parent))))))))

(defn- bindings-vector?
  "True if the vector `outer` is the bindings vector of a `let`-like form or of `doseq` or `for`."
  [outer ns-info]
  (let [head (core-head (z/up outer) ns-info)]
    (and (or (contains? let-heads head) (contains? loop-heads head))
         (= 1 (code-index outer)))))

(defn- binding-scope
  "Return the form a hint on a bound name scopes over, or nil when no known form binds it.

  The name is bound when it sits in a params, fields or bindings vector, also inside
  destructuring there, or as the name of `this-as` or `catch`."
  [outer ns-info]
  (let [parent (z/up outer)
        head (core-head parent ns-info)]
    (if (and (contains? #{"this-as" "catch"} head)
             (= (if (= "catch" head) 2 1) (code-index outer)))
      parent
      (loop [element outer]
        (let [container (z/up element)]
          (cond
            (not (#{:vector :map} (some-> container z/tag))) nil
            (and (= :vector (z/tag container)) (params-vector? (outermost-meta container) ns-info))
            (some-> (outermost-meta container) z/up)
            (and (= :vector (z/tag container)) (bindings-vector? container ns-info))
            (when (even? (code-index element)) (z/up container))
            :else (recur container)))))))

(defn- harmless-position?
  "True if the value written at `outer` surely reaches no interop call from where it stands.

  That is an argument of a plain function call other than an interop target, or an element
  of a literal collection in such a place. A collection right after a head the rule does not
  know could be a bindings vector, and any form that can hand a value on, bind it or thread
  it is a place the rule does not follow, so neither counts."
  [outer ns-info]
  (let [parent (z/up outer)]
    (case (some-> parent z/tag)
      (:list :fn)
      (if (reader-conditional-branch outer)
        (harmless-position? (outermost-meta (z/up parent)) ns-info)
        (let [index (code-index outer)]
          (and (pos? index)
               (case (head-kind parent ns-info)
                 :interop (> index 1)
                 (:core-fn :other) true
                 false))))

      (:vector :set :map)
      (let [container (outermost-meta parent)
            grand (z/up container)]
        (and (not (and (#{:list :fn} (some-> grand z/tag))
                       (= 1 (code-index container))
                       (not= :core-fn (head-kind grand ns-info))))
             (harmless-position? container ns-info)))

      :reader-macro
      (= "js" (some-> parent z/down parser/raw))

      false)))

(defn- name-occurrences
  "Return the locs where the symbol `sym-text` is written in `scope`, one walk per scope and file."
  [sym-text scope]
  (let [^IdentityHashMap by-scope @(parser/file-state ::name-occurrences (IdentityHashMap.))
        index (or (.get by-scope (z/node scope))
                  (let [built (->> scope
                                   parser/descendant-locs
                                   (filter #(= :token (z/tag %)))
                                   (group-by parser/raw))]
                    (.put by-scope (z/node scope) built)
                    built))]
    (get index sym-text)))

(defn- name-unread?
  "True if every use of the local `sym-text`, bound at `binding-loc`, is surely not read through interop.

  A use in compiled code counts as harmless in a [[harmless-position?]], or when it carries a
  tag of its own. Names compare as written, so `other/el` is not the local `el`."
  [sym-text scope binding-loc ns-info]
  (every? #(let [outer (outermost-meta %)]
             (or (identical? (z/node %) (z/node binding-loc))
                 (harmless-position? outer ns-info)
                 (and (not (identical? outer %)) (own-tag? outer))
                 (not (compiled-code? % ns-info))))
          (name-occurrences sym-text scope)))

(defn- hint-unread?
  "True if a `^js` hint surely changes nothing that shadow-cljs infers.

  shadow-cljs reads a hint to type the target of an interop call, also through a name the
  value is bound to, a thread, a branch, or a return. The rule flags a hint only when it can
  see that none of that happens: a hint on a name a known form binds whose every use is
  harmless, or a hint on a value in a harmless position. Anywhere else it leaves the hint."
  [hint-loc ns-info]
  (let [outer (outermost-meta hint-loc)
        hinted (heads/unwrap-meta hint-loc)
        scope (binding-scope outer ns-info)]
    (boolean
     (if scope
       (and (= :token (z/tag hinted))
            (name-unread? (parser/sym-name hinted) scope hinted ns-info))
       (and (not (#{:vector :map} (some-> outer z/up z/tag)))
            (harmless-position? outer ns-info))))))

(defn- js-hint-finding
  "Build the finding for a `^js` hint.

  `:test` is the definition the hint sits in. A hint's own text is short and repeats —
  `^js el` can appear many times in one file — so without it every hint of the same
  shape in a file would be a single baseline identity. Several identical hints inside one
  definition still collapse, which matters little here: the fix removes all of them."
  [hint-loc test-name file]
  (let [[row col] (parser/position hint-loc)]
    {:kw (symbol js-tag)
     :type :js-hint-in-test
     :form (parser/normalize-form (parser/raw hint-loc))
     :test test-name
     :file file
     :row row
     :col col
     :hint "Remove the hint — no interop call reads this value, so shadow-cljs has nothing to infer."}))

(defn- js-hint-in-test
  "Return the finding for a `^js` hint in a test namespace that no interop call reads, or nil."
  [paths loc ns-info file]
  (when (test-file? paths ns-info file)
    (when-let [hint-loc (js-type-hint loc)]
      (when (and (compiled-code? hint-loc ns-info)
                 (hint-unread? hint-loc ns-info))
        (js-hint-finding hint-loc (parser/enclosing-definition-name hint-loc) file)))))

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

(def ^:private no-ns-name
  "The namespace `parser` gives a file without an `ns` form, which every such file shares."
  "unknown")

(defn- string-constant-usage
  "Record the var and value of a top-level `(def name \"…\")`, docstring or not, or nil."
  [loc {:keys [ns-name]}]
  (when (and (= "def" (some-> loc z/down parser/sym-name))
             (= :forms (some-> loc z/up z/tag))
             (not= no-ns-name ns-name))
    (let [children (parser/child-locs loc)
          value (last children)
          var-name (some-> (parser/declared-name-loc loc) parser/sym-name)]
      (when (and var-name (literal-string-loc? value))
        {:type ::string-constant
         :var (keyword ns-name var-name)
         :value (z/sexpr value)}))))

(defn- find-literal-and-symbol
  "Split an `(is (= \"…\" sym))`, in either order, into its literal and symbol locs, or nil."
  [is-loc]
  (when (= "is" (some-> is-loc z/down parser/sym-name))
    (let [comparison (some-> is-loc z/down z/right)]
      (when (and (= :list (some-> comparison z/tag))
                 (= "=" (some-> comparison z/down parser/sym-name)))
        (let [[_ left right :as children] (parser/child-locs comparison)]
          (when (= 3 (count children))
            (cond
              (and (literal-string-loc? left) (parser/sym-name right)) [left right]
              (and (literal-string-loc? right) (parser/sym-name left)) [right left])))))))

(defn- strip-to-name
  "Drop a token's leading colons and namespace, so `:label`, `::label` and `msgs/label` all read `label`."
  [token-text]
  (-> token-text (str/replace #"^:+" "") (str/replace #"^[^/]+/(?=.)" "")))

(defn- count-name-in-top-level-form [loc token-name]
  (->> (tree-seq n/inner? n/children (z/node (parser/top-level-loc loc)))
       (filter #(and (= :token (n/tag %)) (= token-name (strip-to-name (n/string %)))))
       count))

(defn- resolve-constant-ref
  "Resolve the var a symbol names, or nil when anything in its top-level form could rebind it.

  A binding, a `with-redefs` or a `binding` writes the name a second time, as a symbol or a
  keyword, qualified or not, so the symbol counts only when its name appears once."
  [sym-loc {:keys [ns-name aliases refers]}]
  (let [sym-raw (parser/raw sym-loc)
        slash (str/index-of sym-raw "/")
        var-name (if slash (subs sym-raw (inc slash)) sym-raw)]
    (when (= 1 (count-name-in-top-level-form sym-loc var-name))
      (if slash
        (let [prefix (subs sym-raw 0 slash)]
          (keyword (get aliases prefix prefix) var-name))
        (keyword (get refers sym-raw ns-name) sym-raw)))))

(defn- literal-constant-usage
  "Record a test-code assertion comparing a literal with a constant, outside `comment` and `quote`, or nil."
  [paths loc ns-info file]
  (when-let [[literal-loc sym-loc] (find-literal-and-symbol loc)]
    (when-let [var-kw (and (test-file? paths ns-info file)
                           (nil? (enclosing-list-head loc #{"comment"}))
                           (not (inside-written-out-quote? loc))
                           (resolve-constant-ref sym-loc ns-info))]
      {:type ::literal-constant-assertion
       :var var-kw
       :literal (z/sexpr literal-loc)
       :kw (symbol "is")
       :form (parser/normalize-form (parser/raw loc))
       :file file
       :row (parser/position-row loc)
       :hint (str (name var-kw) " is defined as this same string, so the assertion repeats its definition. "
                  "Assert on what the code does with it.")})))

(defn- find-list-finding [paths loc ns-info file]
  (or (leading-article-finding loc file)
      (when-let [message (inline-assertion-message loc)] (assertion-finding loc message file))
      (when-let [conditional (conditional-in-assertion loc)]
        (conditional-finding conditional :computes (parser/sym-name (z/down conditional)) file))
      (when-let [kind (conditional-around-assertion loc)]
        (conditional-finding loc kind (parser/sym-name (z/down loc)) file))
      (var-deref-in-test paths loc ns-info file)))

(defn- handle-list [paths loc ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (let [finding (find-list-finding paths loc ns-info file)
          usage (or (string-constant-usage loc ns-info)
                    (literal-constant-usage paths loc ns-info file))]
      (when (or finding usage)
        {:decls (if finding [finding] [])
         :usages (if usage [usage] [])
         :dynamics []}))))

(defn- find-literal-constant-assertions
  "Keep the assertions of `usages` whose literal is the one string their constant is defined as in `usages-in-run`."
  [usages usages-in-run]
  (let [var->values (->> usages-in-run
                         (filter #(= ::string-constant (:type %)))
                         (reduce (fn [acc {:keys [var value]}] (update acc var (fnil conj #{}) value)) {}))]
    (->> usages
         (filter #(= ::literal-constant-assertion (:type %)))
         (filter #(= #{(:literal %)} (var->values (:var %))))
         (mapv #(dissoc % :type :var :literal)))))

(defn- analyze* [{:keys [declarations usages usages-in-run]}]
  {:literal-constant-assertion (find-literal-constant-assertions usages (or usages-in-run usages))
   :deftest-leading-article (filterv #(= :deftest-leading-article (:type %)) declarations)
   :assertion-message-inline (filterv #(= :assertion-message-inline (:type %)) declarations)
   :conditional-assertion (filterv #(= :conditional-assertion (:type %)) declarations)
   :var-deref-in-test (filterv #(= :var-deref-in-test (:type %)) declarations)
   :js-hint-in-test (filterv #(= :js-hint-in-test (:type %)) declarations)})

(defn- summary-lines* [{:keys [deftest-leading-article assertion-message-inline
                               conditional-assertion var-deref-in-test js-hint-in-test
                               literal-constant-assertion]}]
  [["Deftest leading article:" (count deftest-leading-article)]
   ["Assertion message inline:" (count assertion-message-inline)]
   ["Conditional assertion:" (count conditional-assertion)]
   ["Var deref in test:" (count var-deref-in-test)]
   ["Js hint in test:" (count js-hint-in-test)]
   ["Literal constant assertion:" (count literal-constant-assertion)]])

(defn- failed?* [_]
  ;; Nothing here blocks at runtime, and the guard shape has defensible uses — a `when`
  ;; narrowing a doseq to the combination under test reads as deliberate. A project that
  ;; wants any of these enforced opts in with --fail-on.
  false)

(defrecord TestsGroup [paths extensions]
  group/RuleGroup
  (group-id [_] :tests)
  (group-name [_] "Tests")
  (parse-handlers [_] {:handle-list (fn [loc ns-info file] (handle-list paths loc ns-info file))
                       :handle-token (fn [loc ns-info file]
                                       (when-let [finding (or (var-deref-in-test paths loc ns-info file)
                                                              (js-hint-in-test paths loc ns-info file))]
                                         {:decls [finding]
                                          :usages []
                                          :dynamics []}))})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ result] (failed?* result))
  (suggestions [_]
    {:deftest-leading-article
     (str "A test name opens with \"a-\", \"an-\" or \"the-\". Drop the article: "
          "the-panel-opens becomes panel-opens.")
     :assertion-message-inline
     (str "An assertion message shares a line with the expression it describes. "
          "Move the message to its own line under the expression.")
     :conditional-assertion
     (str "A test works out what to expect while it runs, with a conditional in the expected value, "
          "around assertions, or as a guard that can skip them. Write a table of inputs and expected "
          "values and walk it with doseq.")
     :js-hint-in-test
     (str "A ^js type hint on a value that no interop call reads changes nothing: shadow-cljs reads a hint "
          "only to type the target of an interop call. Remove it.")
     :literal-constant-assertion
     (str "An assertion compares a string literal with the constant defined as that same string, so it repeats the "
          "definition. Assert on what the code does with the constant instead.")
     :var-deref-in-test
     (str "A test reads a var through its var quote, such as @#'ns/x or (deref (var ns/x)). "
          "Call it through the public function that uses it, or move it where it can be public.")})
  (docs-anchors [_]
    {:assertion-message-inline "assertion-message-inline"
     :conditional-assertion "conditional-assertion"
     :deftest-leading-article "deftest-leading-article"
     :js-hint-in-test "js-hint-in-test"
     :literal-constant-assertion "literal-constant-assertion"
     :var-deref-in-test "var-deref-in-test"})
  (rule->tier [_]
    {:deftest-leading-article :cleanup
     :assertion-message-inline :cleanup
     :conditional-assertion :cleanup
     :var-deref-in-test :cleanup
     :js-hint-in-test :cleanup
     :literal-constant-assertion :cleanup})
  (file-extensions [_] extensions))

(defn make-group
  "Return a tests RuleGroup configured with the given map.

  Supported keys:
    :paths [\"test\" …] — directories holding test code. A file under any of them is test
      code whatever its namespace is called. One path may be written on its own rather
      than in a vector. With none set, a namespace whose name ends in `-test` is taken as
      the test, which is what the group does out of the box.

  `clj?` is the experimental `.clj` mode. It is a separate argument rather than a key in
  the map because the map comes straight from the config file and may be anything the
  user wrote there."
  ([] (make-group nil false))
  ([config] (make-group config false))
  ([{:keys [paths]} clj?]
   (->TestsGroup (cond
                   (string? paths) [paths]
                   (coll? paths) (vec paths)
                   :else [])
                 (group/extensions-for clj?))))

(def group (make-group))
