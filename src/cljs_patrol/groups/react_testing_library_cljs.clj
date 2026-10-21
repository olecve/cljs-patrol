(ns cljs-patrol.groups.react-testing-library-cljs
  "Rule group: misuse of react-testing-library-cljs queries in test code."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private throwing-query-namespaces
  "Namespaces whose `get-by-*` and `get-all-by-*` throw when nothing matches.

  react-testing-library-cljs wraps Testing Library's `getBy*` and `getAllBy*`, which throw
  on a miss, in both `screen` and `within`."
  #{"react-testing-library-cljs.screen" "react-testing-library-cljs.within"})

(defn- resolve-head
  "Return the namespace and name a call's head symbol resolves to through the ns form, or nil.
  A head may also name a throwing-query namespace in full, which needs no alias."
  [head {:keys [aliases refers]}]
  (if-let [slash (str/index-of head "/")]
    (let [ns-part (subs head 0 slash)]
      (when-let [full-ns (or (get aliases ns-part) (get throwing-query-namespaces ns-part))]
        [full-ns (subs head (inc slash))]))
    (when-let [full-ns (get refers head)]
      [full-ns head])))

(def ^:private binding-vector-heads
  "Forms whose vector pairs a name with a value, so only the names, every other element, bind."
  #{"let" "let*" "loop" "when-let" "if-let" "when-some" "if-some" "when-first" "binding" "with-redefs"
    "with-open" "doseq" "for" "dotimes" "with-local-vars"})

(defn- vector-tokens [loc]
  (->> loc parser/descendant-locs (cons loc) (filter #(= :token (z/tag %)))))

(defn- bound-name-locs
  "Return the tokens a form binds through its vector: names, parameters, or a letfn's function names."
  [form]
  (let [head (some-> form z/down parser/sym-name)
        vectors (filter #(= :vector (z/tag %)) (take-while some? (iterate z/right (z/down form))))]
    (cond
      (contains? binding-vector-heads head)
      (some->> (first vectors) z/down (iterate z/right) (take-while some?) (take-nth 2) (mapcat vector-tokens))

      (= "letfn" head)
      (some->> (first vectors) z/down (iterate z/right) (take-while some?) (keep z/down))

      (or (contains? #{"fn" "fn*"} head) (some-> head (str/starts-with? "def")))
      (mapcat vector-tokens vectors))))

(defn- rebound?
  "True when a form enclosing `loc` binds `head`, so the name may not be what the ns form says.
  A `let` or `fn` local, a `letfn` function, a `with-redefs` or a `binding` all count; a
  vector that only uses the name, as a value, does not."
  [loc head]
  (boolean (some (fn [form] (some #(= head (parser/raw %)) (bound-name-locs form)))
                 (take-while some? (rest (iterate z/up loc))))))

(defn- throwing-query
  "Return the name of the throwing lookup `loc` calls, as in \"get-by-role\", or nil."
  [loc ns-info]
  (when-let [[full-ns fn-name] (some-> loc z/down parser/raw (resolve-head ns-info))]
    (when (and (contains? throwing-query-namespaces full-ns)
               (re-matches #"get-(all-)?by-[a-z-]+" fn-name))
      fn-name)))

(def ^:private presence-wrappers
  "Heads that only ask whether a lookup found something, which a throwing lookup always has."
  #{"some?" "boolean" "seq" "not-empty"})

(defn- presence-checked-lookup
  "Return the lookup an assertion checks only for presence, or nil.

  The expression is the lookup itself, the lookup inside one of [[presence-wrappers]], or
  the lookup under `(not (nil? …))`. A lookup handed to anything else, `(.-textContent
  (get-by-role …))` or `(count (get-all-by-text …))`, asserts more than that it exists."
  [is-loc]
  (let [expression (some-> is-loc z/down z/right)
        head (some-> expression z/down parser/sym-name)
        argument (some-> expression z/down z/right)
        only-argument? (and argument (nil? (z/right argument)))]
    (cond
      (and (contains? presence-wrappers head) only-argument?) argument
      (and (= "not" head) only-argument? (= "nil?" (some-> argument z/down parser/sym-name))
           (nil? (some-> argument z/down z/right z/right)))
      (some-> argument z/down z/right)
      :else expression)))

(defn- get-by-presence-finding
  "Return the finding for an assertion that only checks a throwing lookup found something, or nil."
  [loc ns-info file]
  (when (= "is" (some-> loc z/down parser/sym-name))
    (when-let [lookup (presence-checked-lookup loc)]
      (when-let [fn-name (throwing-query lookup ns-info)]
        (let [head (parser/raw (z/down lookup))
              query (str (subs head 0 (- (count head) (count fn-name))) "query" (subs fn-name 3))]
          (when-not (or (rebound? lookup head) (rebound? loc (parser/raw (z/down loc))))
            {:kw (symbol fn-name)
             :type :get-by-presence-check
             :form (parser/normalize-form (parser/raw loc))
             :test (parser/enclosing-definition-name loc)
             :file file
             :row (parser/position-row loc)
             :hint (str "Assert presence with "
                        (if (str/starts-with? fn-name "get-all-")
                          (str "(seq (" query " …)), which is empty on a miss")
                          (str query ", which returns nil on a miss"))
                        ", or call " head " outside the is.")}))))))

(defn- handle-list [loc ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (when-let [finding (get-by-presence-finding loc ns-info file)]
      {:decls [finding]
       :usages []
       :dynamics []})))

(defn- analyze* [{:keys [declarations]}]
  {:get-by-presence-check (vec (filter #(= :get-by-presence-check (:type %)) declarations))})

(defn- summary-lines* [{:keys [get-by-presence-check]}]
  [["Get-by presence check:" (count get-by-presence-check)]])

(defrecord ReactTestingLibraryCljsGroup []
  group/RuleGroup
  (group-id [_] :react-testing-library-cljs)
  (group-name [_] "react-testing-library-cljs")
  (parse-handlers [_] {:handle-list handle-list})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ _] false)
  (suggestions [_]
    {:get-by-presence-check
     (str "An assertion only checks that a get-by-* lookup found something, but get-by-* throws on a miss, "
          "so the check can never fail. Assert presence with query-by-* (seq of query-all-by-*), or call "
          "get-by-* outside the is.")})
  (docs-anchors [_]
    {:get-by-presence-check "get-by-presence-check"})
  (rule->tier [_]
    {:get-by-presence-check :cleanup})
  (file-extensions [_] group/cljs-extensions))

(def group (->ReactTestingLibraryCljsGroup))
