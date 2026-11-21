(ns cljs-patrol.dev.mutate.forms
  "Read the target's source: its top-level forms, the mutants they allow, the skip markers, and what a run covers."
  (:require
   [cljs-patrol.dev.mutate.error :as error]
   [cljs-patrol.parser :as parser]
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(def swaps
  "Each operator and the one it is replaced with, both ways."
  (let [pairs [['when 'when-not]
               ['if 'if-not]
               ['and 'or]
               ['= 'not=]
               ['< '<=]
               ['> '>=]
               ['seq 'empty?]
               ['filter 'remove]
               ['some? 'nil?]
               ['true? 'false?]
               ['inc 'dec]
               ['first 'last]
               ['every? 'some]]]
    (into {true false
           false true}
          (mapcat (fn [[a b]] [[a b] [b a]]))
          pairs)))

(def ^:private mutated-heads
  "Definitions that evaluating again replaces, so a mutant of one takes effect."
  #{"def" "defn" "defn-" "defmethod"})

(def loaded-once-heads
  "Definitions Clojure keeps from the first load, or that other code captures then.

  Evaluating a mutant of one proves nothing, so they are not mutated."
  #{"defmacro" "defmulti" "defonce" "defprotocol" "defrecord" "deftype"})

(defn- read-branch
  "Return the form a `#?` or `#?@` reader conditional at `loc` gives Clojure: the first `:clj` or `:default` branch."
  [loc]
  (when (and (= :reader-macro (z/tag loc)) (contains? #{"?" "?@"} (some-> loc z/down z/string)))
    (some->> (some-> loc z/down z/right)
             parser/written-child-locs
             (partition 2)
             (some (fn [[feature form]] (when (contains? #{:clj :default} (z/sexpr feature)) form))))))

(defn- unread-branch?
  "True when `loc` is a branch of a reader conditional that Clojure does not read, or one of its feature keywords."
  [loc]
  (let [conditional (some-> loc z/up z/up)]
    (and (= :reader-macro (some-> conditional z/tag))
         (not (identical? (z/node loc) (some-> (read-branch conditional) z/node))))))

(defn- parameter-vector?
  "True when `loc` is a vector, also one under a type hint such as `^String [x]`."
  [loc]
  (or (= :vector (z/tag loc))
      (and (= :meta (z/tag loc)) (= :vector (some-> loc z/down z/rightmost z/tag)))))

(defn- attr-map?
  "True when `loc` is the attribute map of a `defn`: before its parameter vector, or after its last arity."
  [loc]
  (let [parent (z/up loc)]
    (and (= :map (z/tag loc))
         (= :list (some-> parent z/tag))
         (contains? #{"defn" "defn-"} (parser/sym-name (z/down parent)))
         (not-any? parameter-vector? (take-while some? (iterate z/left loc))))))

(def ^:private definition-heads
  "Every head whose second form names what it defines."
  (into mutated-heads loaded-once-heads))

(defn- name-slot?
  "True when `loc` holds the name a definition declares: a mutant of it would define a second var the restore keeps."
  [loc]
  (let [parent (z/up loc)]
    (and (= :list (some-> parent z/tag))
         (contains? definition-heads (parser/sym-name (z/down parent)))
         (identical? (z/node loc) (some-> parent z/down z/right z/node)))))

(defn- dispatch-value?
  "True when `loc` is the dispatch value of a `defmethod`, which names the method a mutant would add beside the original."
  [loc]
  (let [parent (z/up loc)]
    (and (= :list (some-> parent z/tag))
         (= "defmethod" (parser/sym-name (z/down parent)))
         (identical? (z/node loc) (some-> parent z/down z/right z/right z/node)))))

(defn- skipped?
  "True when `loc` holds no code a test reaches through this namespace, or code a mutant could not put back.

  That is metadata, a `defn`'s attribute map, a `#_`, a quoted form, a `comment`, a
  reader-conditional branch Clojure does not read, a definition's name, and a `defmethod`'s
  dispatch value: a mutant of either would add a var or a method that restoring the form does
  not remove."
  [loc]
  (some (fn [ancestor]
          (let [parent (z/up ancestor)]
            (or (contains? #{:uneval :quote :syntax-quote} (z/tag ancestor))
                (dispatch-value? ancestor)
                (name-slot? ancestor)
                (and (= :list (z/tag ancestor)) (= "comment" (parser/sym-name (z/down ancestor))))
                (and (= :meta (some-> parent z/tag)) (identical? (z/node ancestor) (z/node (z/down parent))))
                (attr-map? ancestor)
                (unread-branch? ancestor))))
        (take-while some? (iterate z/up loc))))

(defn- number-replacement
  "Return the integer to put in place of the integer literal `value` written as `text`, or nil for any other value.

  `0` and `1` swap, and any other integer goes up by one. Only a plain decimal literal counts,
  such as `7` or `-1`, so the mutant keeps the written form: `0x10`, `+1`, `1N`, `1.5` and `1/2`
  are left alone."
  [text value]
  (when (and (int? value) (= text (str value)))
    (case value
      0 1
      1 0
      (inc' value))))

(defn- function-def?
  "True when the definition at `loc`, headed `head`, holds a function that runs when called.

  That is a `defn`, `defn-` or `defmethod`, or a `def` whose value is a `fn` or `#(…)`. A `def`
  that builds data, also with a `fn` inside, is read once and cannot be recorded."
  [loc head]
  (or (contains? #{"defn" "defn-" "defmethod"} head)
      (and (= "def" head)
           (let [value (some-> loc z/down z/rightmost)]
             (or (= :fn (some-> value z/tag))
                 (and (= :list (some-> value z/tag)) (contains? #{"fn" "fn*"} (parser/sym-name (z/down value)))))))))

(defn- replacement
  "Return the value to put in place of the token at `loc`, or nil when no operator swaps it.

  A symbol or boolean in `swaps` gets its pair, and an integer literal a near number when
  `numbers?` is true."
  [loc numbers?]
  (when (= :token (z/tag loc))
    (let [value (try (z/sexpr loc) (catch Exception _ nil))]
      (cond
        (contains? swaps value) (get swaps value)
        numbers? (number-replacement (z/string loc) value)))))

(defn- form-name
  "Name a top-level form by what it defines, or by its head and line, such as `extend-type@12`."
  [loc head]
  (or (when (or (contains? mutated-heads head) (contains? loaded-once-heads head))
        (some-> (parser/declared-name-loc loc) parser/sym-name))
      (str head "@" (first (parser/position loc)))))

(defn- unwrapped
  "Return the forms a top-level form at `loc` stands for, each with the top-level form it was written in.

  Metadata in front of a form is peeled off, and a reader conditional gives the branch Clojure
  reads, a splicing one each form of it."
  [loc]
  (case (z/tag loc)
    :meta (map #(assoc % :wrapper loc) (unwrapped (z/rightmost (z/down loc))))
    :reader-macro (when-let [branch (read-branch loc)]
                    (if (and (= "?@" (z/string (z/down loc))) (= :vector (z/tag branch)))
                      (mapcat unwrapped (parser/written-child-locs branch))
                      (unwrapped branch)))
    [{:loc loc}]))

(defn- const-def?
  "True when the form at `loc` is a `def` whose name is `^:const`: Clojure copies its value into each caller as it compiles."
  [loc head]
  (and (= "def" head)
       (let [name-slot (some-> loc z/down z/right)]
         (and (= :meta (some-> name-slot z/tag))
              (true? (:const (meta (try (z/sexpr name-slot) (catch Exception _ nil)))))))))

(defn top-level-forms
  "Return {:loc … :wrapper … :head … :name … :kind …} for each top-level form of `source` but `ns` and `comment`.

  `:kind` is :mutated for a definition evaluating again replaces, :loaded-once for one it does
  not, :const for a `^:const` def, whose value callers copied as they compiled, and :other for
  any other form, whose effects would run again with every mutant. A form under metadata keeps that metadata as
  its `:wrapper`."
  [source]
  (->> (z/of-string source)
       (iterate z/right)
       (take-while some?)
       (mapcat unwrapped)
       (keep (fn [{:keys [loc]
                   :as form}]
               (when (= :list (z/tag loc))
                 (let [head (parser/sym-name (z/down loc))]
                   (when-not (contains? #{"ns" "comment"} head)
                     (assoc form
                            :head head
                            :name (form-name loc head)
                            :kind (cond
                                    (const-def? loc head) :const
                                    (contains? mutated-heads head) :mutated
                                    (contains? loaded-once-heads head) :loaded-once
                                    :else :other)))))))))

(defn- form-locs
  "Return every loc of the zipper `root`, the root included."
  [root]
  (->> root (iterate z/next) (take-while (complement z/end?))))

(defn- skip-reason
  "Return {:reason …} for `:mutate/skip` on the name or in the attribute map of the definition at `loc`, as written.

  Return nil when neither has the key."
  [loc]
  (let [name-slot (some-> loc z/down z/right)
        attr-map (->> (some-> name-slot z/right)
                      (iterate z/right)
                      (take-while some?)
                      (filter attr-map?)
                      first)]
    (some (fn [metadata]
            (when (contains? metadata :mutate/skip)
              {:reason (:mutate/skip metadata)}))
          [(when (= :meta (some-> name-slot z/tag)) (meta (z/sexpr name-slot)))
           (some-> attr-map z/sexpr)])))

(defn- without-conditions
  "Drop the `{:pre … :post …}` condition map that leads the arity `body`, as Clojure reads one: a map with forms after it."
  [body]
  (if (and (= :map (z/tag (first body))) (next body))
    (next body)
    body))

(defn- arity-bodies
  "Return the body forms of each arity of the function that `root`, a zipper of one definition headed `head`, holds.

  There is one list of locs per arity. Only a `fn` that a `def` holds counts, not a `#(…)`,
  whose whole form is its body. A leading condition map stays, so the body is the forms after it.
  A body that is empty, or a lone `nil`, already gives nil, so it is left out."
  [root head]
  (let [fn-loc (if (= "def" head) (z/rightmost (z/down root)) root)
        skipped-slots (case head
                        ; mutate: skip 1: the fn symbol is no parameter vector and no arity, so keeping it changes nothing
                        "def" 1
                        "defmethod" 3
                        2)]
    (when (= :list (z/tag fn-loc))
      (let [after-name (drop skipped-slots (parser/written-child-locs fn-loc))
            arity-body #(->> % (drop-while (complement parameter-vector?)) next without-conditions)
            bodies (if (some parameter-vector? after-name)
                     [(arity-body after-name)]
                     (for [arity after-name
                           :when (and (= :list (z/tag arity)) (parameter-vector? (z/down arity)))]
                       (arity-body (parser/written-child-locs arity))))]
        (remove (fn [body] (or (empty? body) (= ["nil"] (map z/string body)))) bodies)))))

(defn- body-mutants
  "Return a mutant for each arity body of the function `root`, a zipper of one definition, holds: the body replaced by `nil`.

  Its :from is `body`, so a skip marker and `--apply` name it as `body`, and it sits where the
  body's first form starts."
  [root head]
  (for [body (arity-bodies root head)
        :let [[[row col]] (parser/position-span (first body))
              [_ [end-row end-col]] (parser/position-span (last body))]]
    {:row row
     :col col
     :end-row end-row
     :end-col end-col
     :from "body"
     :to nil
     :swap :body
     :mutant-loc (first body)
     :last-node (z/node (last body))}))

(defn- token-mutants
  "Return a mutant for each token of `root`, a zipper of one definition, that an operator swaps, or an integer when `numbers?`."
  [root numbers?]
  (for [token (form-locs root)
        :let [to (replacement token numbers?)]
        :when (and (some? to) (not (skipped? token)))
        :let [[[row col] [end-row end-col]] (parser/position-span token)
              from (z/string token)]]
    {:row row
     :col col
     :end-row end-row
     :end-col end-col
     :from from
     :to to
     :swap (if (number? to) :number :operator)
     :mutant-loc token}))

(defn mutants
  "Return a mutant for every swap the definitions among the top-level `forms` allow.

  Each is {:def … :row … :col … :end-row … :end-col … :from … :to … :swap … :form-loc …
  :mutant-loc …}. `mutated-text` builds the text of its form only when the mutant runs, and
  `replaced-text` the source it replaces, which ends at :end-row and :end-col, only for --apply.
  :swap is :body for a function body replaced by `nil`, :number for an integer literal and
  :operator for any other swap. Function code is the body of a `defn`, `defn-` or `defmethod`,
  or a `def` whose value is a `fn` or `#(…)`. Only there do integer literals change, and each
  arity of it, but of a `#(…)`, gets a body mutant, listed before the definition's other mutants.
  A definition with `:mutate/skip` on its name or in its attribute map gives each of its mutants
  that reason as :skip-reason."
  [forms]
  (for [{:keys [loc wrapper name kind head]} forms
        :when (= :mutated kind)
        :let [function? (function-def? loc head)
              reason (:reason (skip-reason loc))
              root (z/subzip loc)]
        mutant (concat (when function? (body-mutants root head)) (token-mutants root function?))]
    (assoc mutant
           :def name
           :form-loc loc
           :wrapper wrapper
           :skip-reason reason)))

(defn replaced-text
  "Return the source `mutant` replaces, with each line break read as a `\\n`: its token, or its body up to the last form."
  [{:keys [mutant-loc swap last-node]}]
  (if (= :body swap)
    (->> (iterate z/right* mutant-loc)
         (take-while some?)
         (reduce (fn [text loc]
                   (let [text (str text (z/string loc))]
                     (if (identical? (z/node loc) last-node) (reduced text) text)))
                 ""))
    (z/string mutant-loc)))

(defn mutated-text
  "Return the text of the definition of `mutant` with the mutant in it, without the metadata around it.

  A body mutant puts `nil` in place of the body's first form and removes the forms after it, up
  to the body's last one."
  [{:keys [mutant-loc to swap last-node]}]
  (-> (if (= :body swap)
        (loop [loc (z/replace mutant-loc (n/token-node nil))
               done? (identical? (z/node mutant-loc) last-node)]
          (if done?
            loc
            (let [next-loc (z/right loc)]
              (recur (z/remove next-loc) (identical? (z/node next-loc) last-node)))))
        (z/replace mutant-loc (n/token-node to)))
      z/root
      n/string))

(def ^:private skip-marker
  "A `; mutate: skip OPERATOR: REASON` comment."
  #"^;+\s*mutate: skip\s+(\S+):\s+(\S.*?)\s*$")

(def ^:private skip-marker-like
  "A comment that starts like a skip marker, written right or not."
  #"(?i)^;+\s*mutate\W*skip")

(defn- code-loc?
  "True when `loc` is code that runs: not whitespace, a newline, a comma, a comment or a `#_` form."
  [loc]
  (not (contains? #{:whitespace :newline :comma :comment :uneval} (z/tag loc))))

(defn skip-markers
  "Return each `; mutate: skip OPERATOR: REASON` comment of `source` as {:line … :row … :operator … :reason …}.

  `:line` is the comment's own line, and `:row` the line it skips: its own line when code comes
  before it there, else the line of the next code in the same form, or nil when none follows.
  Only comments count, so the words in a string or docstring are no marker. A comment that
  starts like a marker but does not read as one stops the run."
  [source]
  (let [lines (str/split-lines source)
        root (let [loc (z/of-string source)] (or (z/up loc) loc))]
    (vec (for [loc (->> root
                        (iterate z/next*)
                        (take-while (complement z/end?)))
               :when (= :comment (z/tag loc))
               :let [text (str/trim (z/string loc))]
               :when (re-find skip-marker-like text)
               :let [[line col] (parser/position loc)
                     [_ operator reason] (re-find skip-marker text)]]
           (if-not reason
             (throw (error/setup-error (str "The comment on line " line " reads like a skip marker but is not one."
                                            " Write it as ; mutate: skip OPERATOR: REASON, such as"
                                            " ; mutate: skip first: the list holds one item.")))
             {:line line
              :row (if (str/blank? (subs (nth lines (dec line)) 0 (dec col)))
                     (some->> (iterate z/right* loc)
                              rest
                              (take-while some?)
                              (filter code-loc?)
                              first
                              parser/position
                              first)
                     line)
              :operator operator
              :reason reason})))))

(defn- form-lines [{:keys [loc]}]
  (let [[[start] [end]] (parser/position-span loc)]
    (range start (inc end))))

(defn scope
  "Return the definitions and mutants a run covers, the ones a skip marker leaves out, and the markers that match none.

  Return {:defs-in-scope … :loaded-once … :const-defs … :other-forms … :selected … :ignored … :unused-markers …}.
  The run stops when a definition named with `-d` does not exist, when a comment starts like a
  skip marker but is not one, and when a `:mutate/skip` anywhere in the file is no reason string
  or sits on a form the runner does not mutate. A marker is unused when no mutant of its line has its operator: on
  a full run anywhere in the file, and with `-d` or `-s` only on the lines in scope. With `-s`, a
  mutant runs, or is ignored, when any of its lines from :row to :end-row changed."
  [source {:keys [defs lines]}]
  (let [forms (top-level-forms source)
        wanted (set defs)
        unknown (remove (set (map :name forms)) wanted)]
    (when (seq unknown)
      (throw (error/setup-error (str "No top-level definition named " (str/join ", " (sort unknown)) "."))))
    (doseq [{:keys [loc name kind head]} forms
            :let [{:keys [reason]
                   :as found} (try
                                (skip-reason loc)
                                (catch Exception e
                                  (throw (error/setup-error (str "The metadata of " name " cannot be read: " (ex-message e))))))]
            :when found]
      (cond
        (not= :mutated kind)
        (throw (error/setup-error (str name (if (= :const kind)
                                              " is a ^:const def, whose value callers copy, so the runner does not mutate it"
                                              (str " is a " head ", which the runner does not mutate"))
                                       ", and its :mutate/skip does nothing. Remove it.")))

        (or (not (string? reason)) (str/blank? reason))
        (throw (error/setup-error (str "The :mutate/skip metadata on " name " needs a reason as a string, such as"
                                       " ^{:mutate/skip \"wraps a JDK call\"}.")))))
    (let [full-run? (and (empty? wanted) (not (set? lines)))
          in-scope (filter #(and (or (empty? wanted) (contains? wanted (:name %)))
                                 (or (not (set? lines)) (some lines (form-lines %))))
                           forms)
          of-kind (fn [kind] (filter #(= kind (:kind %)) in-scope))
          markers (skip-markers source)
          marker-reasons (into {} (map (juxt (juxt :row :operator) :reason)) markers)
          marked (fn [{:keys [row from]
                       :as mutant}]
                   (update mutant :skip-reason #(or % (marker-reasons [row from]))))
          in-scope-mutants (map marked (mutants in-scope))
          mutant-swaps (set (map (juxt :row :from) in-scope-mutants))
          in-scope-rows (set (mapcat form-lines in-scope))
          [ignored selected] ((juxt filter remove)
                              :skip-reason
                              (filter #(or (not (set? lines)) (some lines (range (:row %) (inc (:end-row %)))))
                                      in-scope-mutants))]
      {:defs-in-scope (mapv :name (of-kind :mutated))
       :loaded-once (mapv #(str (:name %) " (" (:head %) ")") (of-kind :loaded-once))
       :const-defs (mapv :name (of-kind :const))
       :other-forms (mapv :name (of-kind :other))
       :selected selected
       :ignored ignored
       :unused-markers (vec (for [{:keys [line row operator]} markers
                                  :when (and (or full-run? (contains? in-scope-rows row))
                                             (not (contains? mutant-swaps [row operator])))]
                              (str "line " line " (" operator ")")))})))
