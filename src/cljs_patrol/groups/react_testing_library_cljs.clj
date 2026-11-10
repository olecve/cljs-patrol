(ns cljs-patrol.groups.react-testing-library-cljs
  "Rule group: how test code uses and names react-testing-library-cljs queries."
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

(def ^:private library-namespaces
  "Namespaces holding the library's lookups: `get-by-*`, `query-by-*` and `find-by-*`, and their `-all-` forms.

  react-testing-library-cljs wraps Testing Library's queries in both `screen` and `within`;
  its `get-by-*` and `get-all-by-*` throw on a miss, as `getBy*` and `getAllBy*` do."
  #{"react-testing-library-cljs.screen" "react-testing-library-cljs.within"})

(defn- resolve-head
  "Return the namespace and name a call's head symbol resolves to through the ns form, or nil.
  A head may also name a throwing-query namespace in full, which needs no alias."
  [head {:keys [aliases refers]}]
  (if-let [slash (str/index-of head "/")]
    (let [ns-part (subs head 0 slash)]
      (when-let [full-ns (or (get aliases ns-part) (get library-namespaces ns-part))]
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

(defn- library-lookup
  "Return [kind fn-name] for the library lookup `loc` calls, as in [\"get\" \"get-by-role\"], or nil."
  [loc ns-info]
  (when-let [[full-ns fn-name] (some-> loc z/down parser/raw (resolve-head ns-info))]
    (when (contains? library-namespaces full-ns)
      (when-let [[_ kind] (re-matches #"(get|query|find)(?:-all)?-by-[a-z-]+" fn-name)]
        [kind fn-name]))))

(defn- throwing-query
  "Return the name of the throwing lookup `loc` calls, as in \"get-by-role\", or nil."
  [loc ns-info]
  (let [[kind fn-name] (library-lookup loc ns-info)]
    (when (= "get" kind) fn-name)))

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
             :hint (str head " throws on a miss, so this check never fails. Use "
                        (if (str/starts-with? fn-name "get-all-")
                          (str "(seq (" query " …))")
                          query)
                        ".")}))))))

(def ^:private lookup-kinds
  "Prefixes naming what a lookup does on a miss.
  `get` throws, `query` returns nil, and `find` returns a promise that rejects."
  #{"get" "query" "find"})

(defn- single-arity-body
  "Return the forms of a `defn` body, or nil for a form with several arities.
  The name, a docstring and an attribute map are skipped to reach the parameter vector."
  [defn-loc]
  (loop [loc (some-> defn-loc z/down z/right z/right)]
    (case (some-> loc z/tag)
      (:token :multi-line :map) (recur (z/right loc))
      :meta (when (= :vector (some-> loc z/down z/rightmost z/tag))
              (take-while some? (iterate z/right (z/right loc))))
      :vector (take-while some? (iterate z/right (z/right loc)))
      nil)))

(defn- strip-lookup-prefix [helper-name]
  (or (some (fn [kind]
              (when (str/starts-with? helper-name (str kind "-"))
                (subs helper-name (inc (count kind)))))
            lookup-kinds)
      helper-name))

(defn- lookup-helper-finding
  "Return the finding for a helper whose body is one library lookup and whose name misstates what it does on a miss.

  The prefix is what the caller reads: a `query-` helper is expected to return nil, so one
  that wraps `get-by-*` throws on a caller who asserts absence through it."
  [loc ns-info file]
  (when (contains? #{"defn" "defn-"} (some-> loc z/down parser/sym-name))
    (let [body (remove #(= :uneval (z/tag %)) (single-arity-body loc))
          name-loc (parser/declared-name-loc loc)
          helper-name (some-> name-loc parser/sym-name)]
      (when (and helper-name (= 1 (count body)))
        (when-let [[kind fn-name] (library-lookup (first body) ns-info)]
          (when-not (or (str/starts-with? helper-name (str kind "-"))
                        (rebound? (first body) (parser/raw (z/down (first body)))))
            {:kw (symbol helper-name)
             :type :lookup-helper-name
             :file file
             :row (parser/position-row name-loc)
             :hint (str "Rename to " kind "-" (strip-lookup-prefix helper-name)
                        ", after the " fn-name " lookup it returns.")}))))))

(defn- test-id-arguments
  "Return the locs of the arguments a `*-by-test-id` lookup at `loc` is given from its test id on, or nil.
  A `within` lookup takes its container first, a `screen` one takes the id first."
  [loc ns-info]
  (when-let [[_ fn-name] (library-lookup loc ns-info)]
    (when (str/ends-with? fn-name "-by-test-id")
      (let [[full-ns] (resolve-head (parser/raw (z/down loc)) ns-info)
            arguments (remove #(= :uneval (z/tag %)) (parser/right-siblings (z/down loc)))]
        (seq (drop (if (= "react-testing-library-cljs.within" full-ns) 1 0) arguments))))))

(defn- string-value
  "Return the string `loc` holds when it is a string literal, or nil."
  [loc]
  (when (some-> loc parser/raw (str/starts-with? "\""))
    (z/sexpr loc)))

(defn- normalize-test-id
  "Trim a test id and collapse its whitespace, as Testing Library's default normalizer does before matching."
  [id]
  (-> id str/trim (str/replace #"\s+" " ")))

(def ^:private input-type->role
  "Roles of an `<input>` by its type, for the types that have one.
  A `list` attribute turns a text-like input into a combobox, read separately."
  {"text" "textbox"
   "email" "textbox"
   "tel" "textbox"
   "url" "textbox"
   "search" "searchbox"
   "checkbox" "checkbox"
   "radio" "radio"
   "number" "spinbutton"
   "range" "slider"
   "button" "button"
   "submit" "button"
   "reset" "button"
   "image" "button"})

(def ^:private value-named-input-types
  "Input types whose `value` is their name. An image input is named by its `alt` instead."
  #{"button" "submit" "reset"})

(def ^:private queryable-roles
  "Roles a written `:role` may claim for the element to be found by it.
  `presentation`, `none` and `generic` take the element out of what a role query finds."
  #{"alert" "alertdialog" "button" "checkbox" "combobox" "dialog" "gridcell" "heading" "img" "link" "listbox"
    "menu" "menubar" "menuitem" "menuitemcheckbox" "menuitemradio" "meter" "navigation" "option" "progressbar"
    "radio" "region" "row" "searchbox" "slider" "spinbutton" "status" "switch" "tab" "tablist" "tabpanel"
    "textbox" "tooltip" "tree" "treeitem"})

(def ^:private content-named-roles
  "Roles whose accessible name may come from the text inside the element."
  #{"button" "link" "tab" "option" "menuitem" "menuitemcheckbox" "menuitemradio" "checkbox" "radio"
    "switch" "treeitem" "heading" "tooltip" "row" "gridcell"})

(defn- literal-text
  "Return the text a literal string or keyword value renders as, or nil for anything else."
  [value]
  (let [v (a11y/literal-sexpr value)]
    (cond
      (keyword? v) (name v)
      (string? v) v)))

(defn- read-attr
  "Read how attrs set `k`: `:absent`, `:unknown` when it is written as an expression or a name, or its literal value.
  Reagent leaves out an attribute whose value is nil or false, and an empty string sets nothing a
  role depends on."
  [attrs k]
  (let [v (a11y/literal-sexpr (get attrs k))]
    (cond
      (or (= ::a11y/absent v) (nil? v) (false? v) (= "" v)) :absent
      (or (= ::a11y/non-literal v) (symbol? v)) :unknown
      :else v)))

(defn- select-role
  "Return a `select`'s role, or nil when the scan cannot say.

  Testing Library reads any `size` or `multiple` the DOM keeps as a listbox. React drops a
  `size` below 1 and a `multiple` JavaScript reads as false, such as `0`."
  [attrs]
  (let [multiple (read-attr attrs :multiple)
        size (read-attr attrs :size)
        multiple? (cond
                    (or (= :absent multiple) (= 0 multiple)) false
                    (true? multiple) true)
        sized? (cond
                 (= :absent size) false
                 (number? size) (>= size 1))]
    (when (and (some? multiple?) (some? sized?))
      (if (or multiple? sized?) "listbox" "combobox"))))

(defn- input-role
  "Return an `input`'s role by its type, written out or left to its text default, and its `list`.
  An empty `list` leaves a text input with no role Testing Library knows, and one written as an
  expression leaves it unknown."
  [attrs]
  (let [input-type (read-attr attrs :type)
        role (get input-type->role (if (= :absent input-type) "text" (literal-text (get attrs :type))))
        list-attr (a11y/literal-sexpr (get attrs :list))]
    (cond
      (not (contains? #{"textbox" "searchbox"} role)) role
      (contains? #{::a11y/absent nil false} list-attr) role
      (string? list-attr) (when (seq list-attr) "combobox"))))

(defn- element-role
  "Return the role a Testing Library query finds the element by, or nil when the scan cannot say.

  A written `:role` decides it when it is a literal role a query can find; any other written
  `:role`, an empty one included, leaves no role a query finds. Otherwise the role a
  `:test-id-elements` entry gives the component does, and then the tag. An attribute the role
  turns on, written as an expression, leaves it unknown, and a hidden input has no role at all."
  [tag attrs configured-role]
  (let [role (a11y/literal-sexpr (get attrs :role))]
    (cond
      (and (= :input tag) (= "hidden" (literal-text (get attrs :type)))) nil
      (not (contains? #{::a11y/absent nil false} role)) (get queryable-roles (literal-text (get attrs :role)))
      configured-role configured-role
      :else (case tag
              :button "button"
              :a (let [href (read-attr attrs :href)]
                   (when (string? href) "link"))
              :textarea "textbox"
              :select (select-role attrs)
              :input (input-role attrs)
              nil))))

(defn- configured-element
  "Return the `:test-id-elements` entry of the component heading the vector `loc`, or nil.
  A `some.ns/*` key covers every var of that namespace, and an exact key wins over it. A name a
  local binds anew is not the component."
  [{:keys [ns-info test-id-elements]} loc]
  (when (= :vector (some-> loc z/tag))
    (let [head (some-> loc z/down parser/raw)]
      (when-let [full-sym (some-> head (parser/resolve-full-symbol ns-info))]
        (when-not (some? (hiccup/lexically-bound-value loc head))
          (or (get test-id-elements full-sym)
              (some->> (namespace full-sym) (#(symbol % "*")) (get test-id-elements))))))))

(defn- element-attrs
  "Return the tag, literal attrs and `:role-unknown?` of the element vector `loc`, or nil when they are not written out.

  The tag is an HTML tag, or the one `:test-id-elements` maps a component to, whose props are
  then the element's attrs. A configured prop the component reads to change its element, such
  as an `:href` turning a button into a link, leaves its role unknown.

  An element with no attrs map has no attributes, so its attrs read as empty. A map only partly
  readable, such as a `merge` with an expression, is not written out, and neither is a name in the
  attrs slot, which Reagent reads as attrs when it holds a map."
  [context loc]
  (when (= :vector (some-> loc z/tag))
    (let [html-tag (some-> loc z/down parser/raw hiccup/parse-tag)
          configured (configured-element context loc)
          second-child (some-> loc z/down z/right)
          {:keys [kind attrs]} (hiccup/attrs-info loc)
          attrs (case kind
                  :absent {}
                  :non-map (when (or (= :vector (z/tag second-child)) (some? (string-value second-child))) {})
                  :map attrs
                  nil)]
      (when (and (or (hiccup/html-tag? html-tag) configured) (some? attrs))
        {:tag (if configured (:tag configured) html-tag)
         :element-role (:role configured)
         :attrs attrs
         :role-unknown? (boolean (some #(not= :absent (read-attr attrs %)) (:unknown-with configured)))}))))

(defn- css-keyword [value]
  (when (or (string? value) (keyword? value))
    (-> value name str/trim str/lower-case)))

(defn- styled-hidden?
  "True when a `:style` may hide the element: `display: none`, `visibility: hidden`, or a style the scan cannot read.
  A browser reads the keyword whatever its case and surrounding space."
  [attrs]
  (let [style (get attrs :style)]
    (cond
      (nil? style) false
      (not= :map (z/tag style)) true
      :else (let [style-attrs (hiccup/literal-map style)]
              (or (nil? style-attrs)
                  (let [display (read-attr style-attrs :display)
                        visibility (read-attr style-attrs :visibility)]
                    (or (= :unknown display)
                        (= "none" (css-keyword display))
                        (= :unknown visibility)
                        (contains? #{"hidden" "collapse"} (css-keyword visibility)))))))))

(defn- hidden-element?
  "True when attrs may hide the element from a role query, through `:hidden`, `:aria-hidden` or `:style`."
  [attrs]
  (or (contains? #{:unknown true "true" :true} (read-attr attrs :aria-hidden))
      (not= :absent (read-attr attrs :hidden))
      (styled-hidden? attrs)))

(defn- closed-dialog?
  "True when the element is a native `<dialog>` that is not open, which the browser does not render."
  [tag attrs]
  (and (= :dialog tag) (not (true? (read-attr attrs :open)))))

(def ^:private content-hiding-tags
  "Tags whose content the browser does not render: a `dialog` until it is open, a `template` and a `datalist`."
  #{:dialog :template :datalist})

(defn- may-hide?
  "True when the vector at `loc` may hide what it holds.

  An HTML or configured element does when its attrs may hide it or cannot be read. Any other
  component does when its literal props hide it the way attrs would; what it does with props
  the scan cannot read is its own."
  [context loc]
  (if (or (hiccup/html-tag? (some-> loc z/down parser/raw hiccup/parse-tag))
          (some? (configured-element context loc)))
    (if-let [{:keys [tag attrs]} (element-attrs context loc)]
      (or (hidden-element? attrs)
          (and (contains? content-hiding-tags tag)
               (or (not= :dialog tag) (closed-dialog? tag attrs))))
      true)
    (boolean (when (= :vector (some-> loc z/tag))
               (some-> (hiccup/attrs-info loc) :attrs hidden-element?)))))

(defn- hidden-in-form?
  "True when an element written around `loc` in the same form may hide it."
  [context loc]
  (boolean (some #(may-hide? context %) (take-while some? (rest (iterate z/up loc))))))

(defn- element-children
  "Return the locs of the children an element vector renders, its tag and attrs left out."
  [loc]
  (->> (or (hiccup/attrs-slot loc) (z/down loc))
       z/right
       (iterate z/right)
       (take-while some?)
       (remove #(= :uneval (z/tag %)))))

(defn- rendered-text
  "Return the text the element at `loc` renders, when every child is a literal string or an element showing one.
  An element child is HTML or configured; a configured one with a prop that changes what it shows,
  a component, a call, a conditional or a name may render anything, so any of them makes it nil."
  [context loc]
  (let [parts (map (fn [child]
                     (if (= :vector (z/tag child))
                       (when-let [{:keys [attrs role-unknown?]} (element-attrs context child)]
                         (cond
                           role-unknown? nil
                           (hidden-element? attrs) ""
                           :else (rendered-text context child)))
                       (string-value child)))
                   (element-children loc))]
    (when (every? some? parts)
      (normalize-test-id (str/join " " parts)))))

(defn- literal-name
  "Return the accessible name the element has as a literal the scan can read, or nil.

  A non-blank `:aria-label` names any element, a `value` names a button, submit or reset input,
  and the text it renders names one whose role takes its name from content. An
  `:aria-labelledby` may point at nothing, and Testing Library ignores a `:title` for some roles,
  so neither counts."
  [context role tag attrs loc]
  (let [non-blank #(when (and (string? %) (not (str/blank? %))) (normalize-test-id %))]
    (or (non-blank (read-attr attrs :aria-label))
        (when (and (= :input tag) (contains? value-named-input-types (literal-text (get attrs :type))))
          (non-blank (read-attr attrs :value)))
        (when (contains? content-named-roles role)
          (non-blank (rendered-text context loc))))))

(defn- in-comment?
  "True when a `(comment …)` form encloses `loc`, which only a REPL evaluates."
  [loc]
  (boolean (some #(and (= :list (z/tag %)) (= "comment" (some-> % z/down parser/sym-name)))
                 (take-while some? (rest (iterate z/up loc))))))

(defn- role-and-name
  "Return the role and literal name of the element vector `loc`, or nil when it has no role or name the scan can read."
  [context loc]
  (when-let [{:keys [tag attrs role-unknown?]
              :as element} (element-attrs context loc)]
    (when-let [role (when-not role-unknown? (element-role tag attrs (:element-role element)))]
      (when-let [element-name (literal-name context role tag attrs loc)]
        {:role role
         :name element-name}))))

(defn- string-fragments
  "Return the literal strings written anywhere in the form at `loc`."
  [loc]
  (->> (cons loc (parser/descendant-locs loc))
       (keep string-value)
       vec))

(defn- built-test-id
  "Record what a `:data-testid` given as a call, or a name a binding sets to one, may build.

  A `str` builds an id starting with its first argument and ending with its last, when those
  are literals, and holding every literal between. Any other call may build an id holding any
  literal written in it, and one with none may build any id."
  [value-loc file]
  (let [arguments (when (= "str" (some-> value-loc z/down parser/sym-name))
                    (remove #(= :uneval (z/tag %)) (parser/right-siblings (z/down value-loc))))]
    (if (seq arguments)
      {:type ::built-test-id
       :prefix (or (string-value (first arguments)) "")
       :suffix (if (next arguments) (or (string-value (last arguments)) "") "")
       :middle (->> arguments butlast rest (keep string-value) vec)
       :file file}
      {:type ::built-test-id
       :fragments (string-fragments value-loc)
       :file file})))

(defn- may-build?
  "True when a built test id recorded by [[built-test-id]] may be `id`."
  [{:keys [prefix suffix middle fragments]} id]
  (if prefix
    (and (str/starts-with? id prefix) (str/ends-with? id suffix) (every? #(str/includes? id %) middle))
    (or (empty? fragments) (boolean (some #(str/includes? id %) fragments)))))

(defn- mention [id file]
  {:type ::test-id-mention
   :id (normalize-test-id id)
   :file file})

(defn- handle-token
  "Record a string literal as a mention of a test id it may be, and other test ids that reach the DOM.
  A lookup's own test id is no mention. A keyword or quoted name a `:data-testid` is set to is
  one, and a name bound to a call records what the call may build; a name bound to a string
  needs nothing more, since the string is a mention where it is written."
  [loc ns-info file]
  (let [value (string-value loc)
        parent (z/up loc)
        test-id-value? #(= ":data-testid" (some-> % z/left parser/raw))]
    (cond
      value
      (when-not (and (= :list (some-> parent z/tag))
                     (identical? (z/node loc) (some-> (test-id-arguments parent ns-info) first z/node)))
        {:decls []
         :usages [(mention value file)]
         :dynamics []})

      (and (= :quote (some-> parent z/tag)) (test-id-value? parent))
      {:decls []
       :usages [(mention (parser/raw loc) file)]
       :dynamics []}

      (test-id-value? loc)
      (let [v (a11y/literal-sexpr loc)]
        (cond
          (keyword? v) {:decls []
                        :usages [(mention (name v) file)]
                        :dynamics []}
          (symbol? v) (let [bound (hiccup/lexically-bound-value loc (parser/raw loc))]
                        (when (= :list (some-> bound z/tag))
                          {:decls []
                           :usages [(built-test-id bound file)]
                           :dynamics []})))))))

(defn- handle-vector
  "Record an element written out with a role and a literal name: its literal test id, and whether anything may hide it.
  A role query finds it by that role and name unless it is hidden, discarded, or only in a
  `(comment …)` a REPL evaluates."
  [test-id-elements loc ns-info file]
  (let [context {:ns-info ns-info
                 :test-id-elements test-id-elements}]
    (when-let [{:keys [role]
                :as element} (role-and-name context loc)]
      (let [{:keys [tag attrs]} (element-attrs context loc)]
        {:decls []
         :usages [(assoc element
                         :type ::role-element
                         :role role
                         :test-id (some-> (get attrs :data-testid) string-value normalize-test-id)
                         :hidden? (or (hidden-element? attrs)
                                      (closed-dialog? tag attrs)
                                      (hidden-in-form? context loc)
                                      (hiccup/inside-unrendered-form? loc)
                                      (in-comment? loc))
                         :file file)]
         :dynamics []}))))

(defn- test-id-lookup
  "Record a `*-by-test-id` lookup given only a literal test id, or nil.
  An options map after the id may match by substring or normalize differently, so it is left alone."
  [loc ns-info file]
  (let [[id-loc & more] (test-id-arguments loc ns-info)]
    (when-let [id (some-> id-loc string-value)]
      (when-not (or (seq more) (rebound? loc (parser/raw (z/down loc))))
        {:type ::test-id-lookup
         :id (normalize-test-id id)
         :kw (symbol (second (library-lookup loc ns-info)))
         :form (parser/normalize-form (parser/raw loc))
         :test (parser/enclosing-definition-name loc)
         :file file
         :row (parser/position-row loc)}))))

(defn- find-test-id-queries-on-role
  "Keep the lookups of `usages` whose test id only elements a role query finds alone carry.

  Every mention of the id in `usages-in-run` has to be the test id of a visible element with
  one role and literal name, so a component prop, a constant, a hidden element or one with no
  role or name leaves the lookup alone; so does a built id that may be it. The role and name
  have to belong to no element without that test id, or the role query would find that one too."
  [usages usages-in-run]
  (let [by-type (group-by :type usages-in-run)
        id->mention-count (frequencies (map :id (by-type ::test-id-mention)))
        role-elements (by-type ::role-element)
        id->elements (group-by :test-id (remove :hidden? role-elements))
        role+name->test-ids (->> role-elements
                                 (group-by (juxt :role :name))
                                 (into {} (map (fn [[k elements]] [k (set (map :test-id elements))]))))
        built (by-type ::built-test-id)
        may-be-built? (fn [id] (some #(may-build? % id) built))]
    (->> usages
         (filter #(= ::test-id-lookup (:type %)))
         (keep (fn [{:keys [id]
                     :as lookup}]
                 (let [elements (get id->elements id)
                       role+names (set (map (juxt :role :name) elements))
                       [role] (first role+names)]
                   (when (and (= (count elements) (get id->mention-count id))
                              (= 1 (count role+names))
                              (= #{id} (get role+name->test-ids (first role+names)))
                              (not (may-be-built? id)))
                     (-> lookup
                         (dissoc :type :id)
                         (assoc :type :test-id-query-on-role
                                :hint (str "Query it by role \"" role "\" and its name.")))))))
         vec)))

(defn- handle-list [loc ns-info file]
  (when-not (hiccup/inside-unrendered-form? loc)
    (let [finding (or (get-by-presence-finding loc ns-info file)
                      (lookup-helper-finding loc ns-info file))
          usages (keep identity [(test-id-lookup loc ns-info file)
                                 (when (= ":data-testid" (some-> (z/left loc) parser/raw))
                                   (built-test-id loc file))])]
      (when (or finding (seq usages))
        {:decls (if finding [finding] [])
         :usages (vec usages)
         :dynamics []}))))

(defn- analyze* [{:keys [declarations usages usages-in-run]}]
  {:get-by-presence-check (vec (filter #(= :get-by-presence-check (:type %)) declarations))
   :lookup-helper-name (vec (filter #(= :lookup-helper-name (:type %)) declarations))
   :test-id-query-on-role (find-test-id-queries-on-role usages (or usages-in-run usages))})

(defn- summary-lines* [{:keys [get-by-presence-check lookup-helper-name test-id-query-on-role]}]
  [["Get-by presence check:" (count get-by-presence-check)]
   ["Lookup helper name:" (count lookup-helper-name)]
   ["Test id query on role:" (count test-id-query-on-role)]])

(defrecord ReactTestingLibraryCljsGroup [test-id-elements]
  group/RuleGroup
  (group-id [_] :react-testing-library-cljs)
  (group-name [_] "react-testing-library-cljs")
  (parse-handlers [_] {:handle-list handle-list
                       :handle-token handle-token
                       :handle-vector (fn [loc ns-info file] (handle-vector test-id-elements loc ns-info file))})
  (analyze [_ data] (analyze* data))
  (summary-lines [_ result] (summary-lines* result))
  (failed? [_ _] false)
  (suggestions [_]
    {:get-by-presence-check
     (str "An assertion only checks that a get-by-* lookup found something, but get-by-* throws on a miss, "
          "so the check can never fail. Assert presence with query-by-* (seq of query-all-by-*), or call "
          "get-by-* outside the is.")
     :lookup-helper-name
     (str "A helper that only returns a lookup is not named for it. Start its name with what the lookup does on a "
          "miss: get- for get-by-* (throws), query- for query-by-* (nil), find- for find-by-* (rejects).")
     :test-id-query-on-role
     (str "A lookup finds an element by its test id, but that element has a role and a name, so the test can find it "
          "the way a screen reader user does. Query it by role and name, which also checks the name stays.")})
  (docs-anchors [_]
    {:get-by-presence-check "get-by-presence-check"
     :lookup-helper-name "lookup-helper-name"
     :test-id-query-on-role "test-id-query-on-role"})
  (rule->tier [_]
    {:get-by-presence-check :cleanup
     :lookup-helper-name :cleanup
     :test-id-query-on-role :cleanup})
  (file-extensions [_] group/cljs-extensions))

(defn- read-unknown-with [value]
  (cond
    (nil? value) #{}
    (keyword? value) #{value}
    (and (coll? value) (every? keyword? value)) (set value)
    :else (throw (ex-info (str ":react-testing-library-cljs :test-id-elements :unknown-with must be a keyword or a "
                               "collection of keywords, got " (pr-str value))
                          {:value value}))))

(defn- read-test-id-element
  "Read a `:test-id-elements` value as `{:tag … :role … :unknown-with #{…}}`, from a bare tag or a map.
  An entry needs a tag, a role, or both; a role, a string or a keyword, has to be one a query finds an element by."
  [k value]
  (let [entry (cond-> (if (map? value) value {:tag value})
                (keyword? (:role value)) (update :role name))
        {:keys [tag role]} entry]
    (when-not (or (keyword? tag) (some? role))
      (throw (ex-info (str ":react-testing-library-cljs :test-id-elements maps " k " to no tag or role: "
                           (pr-str value))
                      {:key k
                       :value value})))
    (when (and (some? tag) (not (keyword? tag)))
      (throw (ex-info (str ":react-testing-library-cljs :test-id-elements :tag must be a keyword, got " (pr-str tag))
                      {:key k
                       :value value})))
    (when (and (some? role) (not (contains? queryable-roles role)))
      (throw (ex-info (str ":react-testing-library-cljs :test-id-elements :role must be a role a query finds, got "
                           (pr-str role))
                      {:key k
                       :value value})))
    (assoc entry :unknown-with (read-unknown-with (:unknown-with entry)))))

(defn- read-test-id-elements
  "Read the `:test-id-elements` config, refusing a key that is no fully qualified symbol, which no call resolves to."
  [test-id-elements]
  (when-not (or (nil? test-id-elements) (map? test-id-elements))
    (throw (ex-info (str ":react-testing-library-cljs :test-id-elements must be a map, got "
                         (pr-str test-id-elements))
                    {:value test-id-elements})))
  (into {}
        (map (fn [[k v]]
               (when-not (and (symbol? k) (namespace k))
                 (throw (ex-info (str ":react-testing-library-cljs :test-id-elements keys must be fully qualified "
                                      "symbols, got " (pr-str k))
                                 {:key k})))
               [k (read-test-id-element k v)]))
        test-id-elements))

(defn make-group
  "Return a react-testing-library-cljs RuleGroup configured with the given map.
  Supported keys:
    :test-id-elements {fully-qualified-symbol tag-keyword}: a component that puts its
      `:data-testid` on an element of that tag, built from its props as attrs and its children
      as content. The value may instead be {:tag tag-keyword :role role-string :unknown-with #{prop …}},
      with a tag, a role, or both, naming the role of an element no tag says, and the props that
      change which element it renders or what it shows, such as an `:href`."
  ([] (make-group nil))
  ([{:keys [test-id-elements]}]
   (->ReactTestingLibraryCljsGroup (read-test-id-elements test-id-elements))))

(def group (make-group))
