(ns cljs-patrol.hiccup-test
  (:require
   [cljs-patrol.hiccup :as hiccup]
   [cljs-patrol.parser :as parser]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(deftest parse-tag-test
  (testing "plain tag"
    (is (= :img (hiccup/parse-tag ":img"))))

  (testing "tag with class"
    (is (= :img (hiccup/parse-tag ":img.hero")))
    (is (= :div (hiccup/parse-tag ":div.a.b.c"))))

  (testing "tag with id"
    (is (= :img (hiccup/parse-tag ":img#logo"))))

  (testing "tag with class and id in either order"
    (is (= :img (hiccup/parse-tag ":img.hero#logo")))
    (is (= :img (hiccup/parse-tag ":img#logo.hero"))))

  (testing "returns nil for non-keyword tokens"
    (is (nil? (hiccup/parse-tag "img")))
    (is (nil? (hiccup/parse-tag "foo")))
    (is (nil? (hiccup/parse-tag ""))))

  (testing "returns nil for namespaced or aliased keywords"
    (is (nil? (hiccup/parse-tag "::img")))
    (is (nil? (hiccup/parse-tag "::alias/img")))
    (is (nil? (hiccup/parse-tag ":my.ns/img"))))

  (testing "returns nil when the string is only a colon"
    (is (nil? (hiccup/parse-tag ":"))))

  (testing "treats bare :.class / :#id shorthand as :div (Hiccup convention)"
    (is (= :div (hiccup/parse-tag ":.card")))
    (is (= :div (hiccup/parse-tag ":#header")))
    (is (= :div (hiccup/parse-tag ":.a.b")))
    (is (= :div (hiccup/parse-tag ":.a#b")))
    (is (= :div (hiccup/parse-tag ":#id.class")))))

(defn- map-zloc [s]
  (z/of-string s))

(deftest literal-map-test
  (testing "returns {kw → value-loc} for a plain literal map"
    (let [result (hiccup/literal-map (map-zloc "{:src \"x\" :alt \"cat\"}"))]
      (is (= #{:src :alt} (set (keys result))))
      (is (every? (fn [[_ v]] (some? v)) result))))

  (testing "empty map returns empty map"
    (is (= {} (hiccup/literal-map (map-zloc "{}")))))

  (testing "returns nil for a map with a computed (non-keyword) key"
    (is (nil? (hiccup/literal-map (map-zloc "{(compute) 1 :alt \"cat\"}"))))
    (is (nil? (hiccup/literal-map (map-zloc "{\"str-key\" 1}"))))
    (is (nil? (hiccup/literal-map (map-zloc "{'sym-key 1}")))))

  (testing "namespaced keys are preserved via z/sexpr"
    (let [result (hiccup/literal-map (map-zloc "{:ns/alt \"a\" ::local \"b\"}"))]
      (is (contains? result :ns/alt))
      (is (not (contains? result :alt))
          "::local resolves to a namespaced kw, not the plain :alt"))))

(defn- vec-zloc [s]
  (z/of-string s))

(deftest attrs-info-test
  (testing "no children"
    (is (= :absent (:kind (hiccup/attrs-info (vec-zloc "[:img]"))))))

  (testing "literal map — attrs returned"
    (let [info (hiccup/attrs-info (vec-zloc "[:img {:src \"x\" :alt \"cat\"}]"))]
      (is (= :map (:kind info)))
      (is (= #{:src :alt} (-> info :attrs keys set)))))

  (testing "literal empty map"
    (let [info (hiccup/attrs-info (vec-zloc "[:img {}]"))]
      (is (= :map (:kind info)))
      (is (= {} (:attrs info)))))

  (testing "literal map with computed key → :map but attrs nil"
    (let [info (hiccup/attrs-info (vec-zloc "[:img {(compute) 1}]"))]
      (is (= :map (:kind info)))
      (is (nil? (:attrs info)))))

  (testing "non-map literal child (string, keyword, vector) → :non-map"
    (is (= :non-map (:kind (hiccup/attrs-info (vec-zloc "[:img \"caption\"]")))))
    (is (= :non-map (:kind (hiccup/attrs-info (vec-zloc "[:img [:span]]"))))))

  (testing "list / fn call as attrs → :dynamic"
    (is (= :dynamic (:kind (hiccup/attrs-info (vec-zloc "[:img (merge x y)]"))))))

  (testing "reader macro (e.g. #js) as attrs → :dynamic"
    (is (= :dynamic (:kind (hiccup/attrs-info (vec-zloc "[:img #js {:src \"x\"}]"))))))

  (testing "meta-wrapped attrs → :dynamic (conservative — silent)"
    (is (= :dynamic (:kind (hiccup/attrs-info (vec-zloc "[:img ^:foo {:src \"x\"}]")))))))

(defn- img-attrs-info
  "Classify the attrs slot of the [:img …] vector nested anywhere in src."
  [src]
  (loop [loc (z/of-string src)]
    (cond
      (z/end? loc) nil

      (and (= :vector (z/tag loc))
           (= ":img" (some-> loc z/down z/string)))
      (hiccup/attrs-info loc)

      :else (recur (z/next loc)))))

(deftest attrs-info-bound-symbol-test
  (testing "a symbol bound to a map literal classifies as that literal"
    (let [info (img-attrs-info "(let [props {:src \"x\" :alt \"cat\"}] [:img props])")]
      (is (= :map (:kind info)))
      (is (= #{:src :alt} (-> info :attrs keys set)))))

  (testing "when-let and if-let bind the same way"
    (is (= :map (:kind (img-attrs-info "(when-let [props {:alt \"cat\"}] [:img props])"))))
    (is (= :map (:kind (img-attrs-info "(if-let [props {:alt \"cat\"}] [:img props] nil)")))))

  (testing "the innermost binding wins"
    (let [info (img-attrs-info "(let [props {:src \"x\"}] (let [props {:alt \"cat\"}] [:img props]))")]
      (is (= #{:alt} (-> info :attrs keys set))
          "the inner map answers, not the outer one")))

  (testing "resolves past an enclosing form that binds nothing of that name"
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (fn [] [:img props]))")))
        "a form-2 component closes over the binding")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (for [item items] [:img props]))")))
        "for binds item, not props"))

  (testing "a bound map-building call is read like the same call written in the slot"
    (let [info (img-attrs-info "(let [props (merge base {:alt \"cat\"})] [:img props])")]
      (is (= :dynamic-map (:kind info)))
      (is (= #{:alt} (-> info :attrs keys set))
          "a partial view: it answers that :alt is present, never that a key is absent"))
    (is (= :dynamic-map (:kind (img-attrs-info "(let [props (assoc base :alt \"cat\")] [:img props])")))
        "assoc names the key too"))

  (testing "a bound construction naming no key holds an unknown map, not an empty one"
    (is (= :dynamic (:kind (img-attrs-info "(let [props (merge defaults opts)] [:img props])")))
        "the same expression written in the slot classifies the same way")
    (is (= :dynamic (:kind (img-attrs-info "(let [props (assoc base k v)] [:img props])")))
        "a computed key names nothing we can read"))

  (testing "a binding that is not a map literal leaves the slot as it was"
    (is (= :non-map (:kind (img-attrs-info "(let [props (build-props)] [:img props])")))
        "bound to a call naming no keys")
    (is (= :non-map (:kind (img-attrs-info "(let [props (dissoc base :side)] [:img props])")))
        "dissoc says nothing about what is left")
    (is (= :non-map (:kind (img-attrs-info "(let [props other] [:img props])")))
        "bound to another symbol")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"} props (build-props)] [:img props])")))
        "rebound later in the same binding vector"))

  (testing "a shadowing binding ends the search rather than deferring to an outer one"
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (fn [props] [:img props]))")))
        "a fn parameter shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (fn ([] nil) ([props] [:img props])))")))
        "a parameter of one arity of a multi-arity fn shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (let [{:keys [props]} x] [:img props]))")))
        "a destructuring form shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (doseq [props items] [:img props]))")))
        "a doseq binding shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (dotimes [props 3] [:img props]))")))
        "a dotimes binding shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (reify Object (render [this props] [:img props])))")))
        "a method parameter shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (extend-type X P (render [this props] [:img props])))")))
        "a method parameter of an extend-type shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (try 1 (catch js/Error props [:img props])))")))
        "a catch binding shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (as-> (f) props [:img props]))")))
        "an as-> binding shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (this-as props [:img props]))")))
        "a this-as binding shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (r/with-let [props (r/atom nil)] [:img props]))")))
        "a with-let binding shadows, qualified or not")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (go-loop [props 1] [:img props]))")))
        "a go-loop binding shadows")
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (deftype T [props] P (render [this] [:img props])))")))
        "a deftype field binds for every method of the type"))

  (testing "a binding that reaches only one branch does not answer for the other"
    (is (= :map (:kind (img-attrs-info "(if-let [props {:alt \"cat\"}] [:img props] nil)")))
        "the then branch sees the binding")
    (is (= :non-map (:kind (img-attrs-info "(if-let [props {:src \"x\"}] nil [:img props])")))
        "the else branch runs with the symbol unbound"))

  (testing "when-first binds the first element of a collection, not the collection"
    (is (= :non-map (:kind (img-attrs-info "(when-first [props {:alt \"cat\"}] [:img props])")))))

  (testing "a body that begins with a vector is not a parameter list"
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (fn [] ([:img props])))")))
        "the arity scan stops once a parameter vector is found")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (fn ([] [:img props]) ([x] nil)))")))
        "no arity of the fn binds props"))

  (testing "only bindings already made when the usage is read are in scope"
    (is (= :non-map (:kind (img-attrs-info "(let [thumb [:img props] props {:alt \"cat\"}] thumb)")))
        "props is bound to the right of the usage")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"} thumb [:img props] props (f)] thumb)")))
        "the binding left of the usage is the one it sees"))

  (testing "a qualified symbol is not a local binding"
    (is (= :non-map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] [:img ui/props])")))))

  (testing "an unbound symbol stays :non-map"
    (is (= :non-map (:kind (img-attrs-info "[:img props]")))))

  (testing "a bound map literal with a computed key is classified like an inline one"
    (let [info (img-attrs-info "(let [props {(compute) 1}] [:img props])")]
      (is (= :map (:kind info)))
      (is (nil? (:attrs info))))))

(deftest attrs-info-construction-completeness-test
  (testing "a construction whose every part is readable states the whole map"
    (is (= :map (:kind (img-attrs-info "[:img (assoc {:class \"c\"} :alt \"cat\")]")))
        "a literal base and literal keys leave nothing unknown")
    (is (= :map (:kind (img-attrs-info "[:img (merge {:src \"a\"} {:alt \"cat\"})]")))
        "merging literals states the whole map too"))

  (testing "a construction with an unreadable part states only a floor"
    (is (= :dynamic-map (:kind (img-attrs-info "(defn v [base] [:img (assoc base :alt \"cat\")])")))
        "an opaque base may still hold keys of its own")
    (is (= :dynamic-map (:kind (img-attrs-info "(defn v [o] [:img (merge {:src \"a\"} o)])")))
        "merging something opaque in")
    (is (= :dynamic-map (:kind (img-attrs-info "[:img (assoc {:src \"a\"} k v)]")))
        "a computed key names something we cannot read")
    (is (= :dynamic-map (:kind (img-attrs-info "[:img (assoc-in {:src \"a\"} [:a :b] 1)]")))
        "a deeper path leaves the outer key holding a map we did not read"))

  (testing "a one-key assoc-in path is an assoc"
    (is (= :map (:kind (img-attrs-info "[:img (assoc-in {:src \"a\"} [:alt] \"cat\")]")))))

  (testing "a deeper assoc-in path states no value for the key it nests under"
    (let [info (img-attrs-info "[:img (assoc-in {:src \"a\"} [:alt :text] \"cat\")]")]
      (is (= #{:src} (-> info :attrs keys set))
          "recording the nested value under :alt would read as the alt text itself"))))

(deftest attrs-info-def-test
  (testing "a def in the same file names a map"
    (let [info (img-attrs-info "(def props {:src \"a\" :alt \"cat\"}) (defn v [] [:img props])")]
      (is (= :map (:kind info)))
      (is (= #{:src :alt} (-> info :attrs keys set))))
    (is (= :map (:kind (img-attrs-info "(defonce props {:alt \"cat\"}) (defn v [] [:img props])")))
        "defonce binds a var the same way")
    (is (= :map (:kind (img-attrs-info "(def ^:private props {:alt \"cat\"}) (defn v [] [:img props])")))
        "metadata sits between the head and the name")
    (is (= :map (:kind (img-attrs-info "(def props \"doc\" {:alt \"cat\"}) (defn v [] [:img props])")))
        "a docstring sits between the name and the value"))

  (testing "a local of the same name wins, and never falls through to the var"
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defn v [props] [:img props])")))
        "a parameter shadows the var")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defn v [] (let [props (f)] [:img props]))")))
        "a let binding shadows the var, unreadable value and all"))

  (testing "a macro we do not know still binds its parameters"
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defnc row [props] [:img props])")))
        "a defnc parameter shadows the var of the same name")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (rum/defc row < rum/static [props] [:img props])")))
        "the name is read past the namespace and past the mixin")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defui App [this props] [:img props])")))
        "an unknown macro leaves the symbol unknown rather than letting the var answer")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (fn-traced [props] [:img props])")))
        "a head that looks nothing like a definition binds all the same")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defnc row (^:x [props] [:img props]))")))
        "one arity of a multi-arity macro, with metadata on its parameter list")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defnc row [^js props] [:img props])")))
        "metadata rides along on a parameter without hiding it")
    (is (= :map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (component [props on-select] [:img props])")))
        "known limit: a macro named neither def… nor …fn… is left unread, so the var answers"))

  (testing "a vector of symbols in a form that defines nothing is data"
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (if wide? [wide-photo props] [:img props]))")))
        "a Reagent component vector in an if arm is not a parameter list")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (use-memo (fn [] x) [props]) [:img props])")))
        "nor is a hook dependency vector")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (into [[card props]] [[:img props]]))")))
        "nor a nested component vector"))

  (testing "a vector holding something no parameter list holds is data, not parameters"
    (let [info (img-attrs-info "(def props {:alt \"cat\"}) (def thumbnail [:img props])")]
      (is (= :map (:kind info))
          "a def whose value is Hiccup does not bind its own tag as a parameter"))
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (list [:span props] [:img props]))")))
        "nor does a Hiccup vector written before this one in the same call")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (if open? [:span props] [:img props]))")))
        "nor does the other arm of an if")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (my-layout [sidebar [:img props]]))")))
        "and the vector the usage itself sits in is the form's body, not its parameters"))

  (testing "metadata in front of a parameter list does not hide it"
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defn v ^:static [props] (render [:img props]))")))
        "the parameters bind whatever is attached to them, and the var must not answer"))

  (testing "the last def of a name wins, the way a re-def rebinds the var"
    (let [info (img-attrs-info "(def props {:alt \"cat\"}) (def props {:src \"a\"}) (defn v [] [:img props])")]
      (is (= #{:src} (-> info :attrs keys set)))))

  (testing "metadata stacks in front of the name"
    (is (= :map (:kind (img-attrs-info "(def ^:private ^:const props {:alt \"cat\"}) (defn v [] [:img props])")))
        "two metadata layers")
    (is (= :map (:kind (img-attrs-info "(def ^{:doc \"d\"} ^:private props {:alt \"cat\"}) (defn v [] [:img props])")))
        "a metadata map and a shorthand layer"))

  (testing "a def of something other than a map literal answers nothing"
    (is (= :non-map (:kind (img-attrs-info "(def props (make-props)) (defn v [] [:img props])")))))

  (testing "a def binds its last form once #_ forms are dropped"
    (is (= #{:alt} (-> (img-attrs-info "(def props {:alt \"cat\"} #_{:src \"a\"}) (defn v [] [:img props])") :attrs keys set))
        "trailing discarded map")
    (is (= :non-map (:kind (img-attrs-info "(def props #_{:alt \"cat\"}) (defn v [] [:img props])")))
        "only discarded value")
    (is (= #{:alt} (-> (img-attrs-info "(def props {:alt \"cat\"}) (def props) (defn v [] [:img props])") :attrs keys set))
        "re-def without value keeps earlier value"))

  (testing "a form other than def binds nothing"
    (is (not= :map (:kind (img-attrs-info "(defn props [] {:alt \"cat\"}) (defn v [] [:img props])")))
        "defn returning map"))

  (testing "a construction over a defined base is readable end to end"
    (let [info (img-attrs-info "(def base {:src \"a\"}) (defn v [] [:img (assoc base :alt \"cat\")])")]
      (is (= :map (:kind info))
          "nothing about the map is unknown, so absence can be asserted from it")
      (is (= #{:src :alt} (-> info :attrs keys set))))))

(deftest attrs-slot-test
  (testing "a literal map occupies the attrs slot"
    (is (some? (hiccup/attrs-slot (vec-zloc "[:button {:on-click f}]")))))

  (testing "a symbol bound to a map literal occupies it too"
    (let [loc (-> (z/of-string "(let [props {:on-click f}] [:button props])") z/down z/rightmost)]
      (is (some? (hiccup/attrs-slot loc))
          "the body starts after the props symbol, so the button counts as empty")))

  (testing "a call the attrs were read from occupies the slot, not the body"
    (is (some? (hiccup/attrs-slot (vec-zloc "[:button (assoc {:class \"c\"} :on-click f)]")))
        "a construction read whole is the attrs slot, so the button counts as empty")
    (let [loc (-> (z/of-string "(defn v [base f] [:button (assoc base :on-click f)])") z/down z/rightmost)]
      (is (some? (hiccup/attrs-slot loc))
          "a floor is still attrs — what it cannot answer is a separate question")))

  (testing "nothing occupies the slot when it cannot be read"
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button]"))))
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button \"Save\"]"))))
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button (build-props)]"))))
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button props]"))))))

(deftest props-slot-test
  (let [slot-string #(some-> (hiccup/props-slot (vec-zloc %)) z/string)]

    (testing "whatever attrs-slot can read is the props slot too"
      (is (= "{:on-click f}" (slot-string "[:button {:on-click f}]")))
      (is (= "(assoc base :on-click f)" (slot-string "[:button (assoc base :on-click f)]"))))

    (testing "a call attrs cannot be read out of still occupies the slot"
      (is (nil? (hiccup/attrs-slot (vec-zloc "[:button (build-props)]")))
          "this is the case the two differ on, and the reason props-slot exists")
      (is (= "(build-props)" (slot-string "[:button (build-props)]"))))

    (testing "a call that yields markup is a body child, not props"
      (is (nil? (slot-string "[:button (when open? [:a \"x\"])]")))
      (is (nil? (slot-string "[:button (if open? [:a \"x\"] [:b \"y\"])]"))
          "every branch yields a vector, so the call renders where it is written"))

    (testing "only a call is weighed this way"
      (is (nil? (slot-string "[:button props]"))
          "a bare symbol is far more often the first body child")
      (is (nil? (slot-string "[:button '(1 2)]"))
          "a quoted list is data, not a call")
      (is (nil? (slot-string "[:button \"Save\"]"))))

    (testing "nothing in the slot when there is no second child"
      (is (nil? (slot-string "[:button]"))))

    (testing "the kind and the second child's shape agree about what a call is"
      (doseq [source ["[:button (build-props)]" "[:button (when open? [:a \"x\"])]"]]
        (let [loc (vec-zloc source)]
          (is (= :list (z/tag (-> loc z/down z/right)))
              (str ":dynamic is only reached through a list second child, in " source))))
      (is (not= :dynamic (:kind (hiccup/attrs-info (vec-zloc "[:button props]"))))
          "a token second child classifies as :non-map, which is why a bare symbol never reaches the call branch"))))

(deftest inside-unrendered-form?-test
  (testing "true when the vector's immediate parent is a quote-family node"
    (let [zloc (-> (z/of-string "'[:img]") z/down)]
      (is (hiccup/inside-unrendered-form? zloc))))

  (testing "true for syntax-quoted vectors"
    (let [zloc (-> (z/of-string "`[:img]") z/down)]
      (is (hiccup/inside-unrendered-form? zloc))))

  (testing "true for a discarded vector"
    (let [zloc (-> (z/of-string "[#_[:img] \"shown\"]") z/down)]
      (is (hiccup/inside-unrendered-form? (z/down zloc)))))

  (testing "true for a vector nested below the quoted one"
    (let [zloc (-> (z/of-string "'[:div [:img]]") z/down z/down z/right)]
      (is (hiccup/inside-unrendered-form? zloc))
      (is (= "[:img]" (z/string zloc))
          "the loc under test is the inner vector, two levels below the quote")))

  (testing "false for a plain top-level vector"
    (is (not (hiccup/inside-unrendered-form? (z/of-string "[:img]")))))

  (testing "false when parent is another vector"
    (let [zloc (-> (z/of-string "[[:img]]") z/down)]
      (is (not (hiccup/inside-unrendered-form? zloc))))))

(defn- loc-at
  "Return the loc of the first form in source whose text is target."
  [source target]
  (z/find (z/of-string source) z/next #(= target (z/string %))))

(deftest enclosing-forms-names-each-kind
  (is (= #{:unrendered} (hiccup/enclosing-forms (loc-at "'[:div [:img]]" "[:img]")))
      "quote two levels up")
  (is (= #{:style-decl} (hiccup/enclosing-forms (loc-at "(defclass c [] [:a {:color :red}])" "[:a {:color :red}]")))
      "style declaration")
  (is (= #{:ns} (hiccup/enclosing-forms (loc-at "(ns a (:require [b.c :as d]))" "[b.c :as d]")))
      "ns form")
  (is (= #{} (hiccup/enclosing-forms (loc-at "(defn v [] [:div [:img]])" "[:img]")))
      "nothing for rendered markup")
  (is (= #{} (hiccup/enclosing-forms (z/of-string "[:img]")))
      "nothing at top level"))

(deftest enclosing-forms-collects-nested-kinds
  (is (= #{:unrendered :style-decl :ns}
         (hiccup/enclosing-forms (loc-at "(ns a '(defclass c [] [:b]))" "[:b]")))
      "every kind on path")
  (is (= #{:unrendered :style-decl}
         (hiccup/enclosing-forms (loc-at "(comment (defclass c [] '[:a [:b]]))" "[:b]")))
      "comment form counts as no kind"))

(deftest enclosing-forms-reads-heads-of-lists-only
  (is (= #{} (hiccup/enclosing-forms (loc-at "[defclass [:a]]" "[:a]")))
      "vector starting with style macro name")
  (is (= #{} (hiccup/enclosing-forms (loc-at "[ns [:a]]" "[:a]")))
      "vector starting with ns"))

(deftest inside-predicates-answer-own-kind
  (let [quoted (loc-at "'[:div [:img]]" "[:img]")
        styled (loc-at "(defattrs c [] [:a {:color :red}])" "[:a {:color :red}]")
        required (loc-at "(ns a (:require [b.c :as d]))" "[b.c :as d]")
        rendered (loc-at "(defn v [] [:div [:img]])" "[:img]")]
    (is (= [true false false false] (mapv hiccup/inside-unrendered-form? [quoted styled required rendered]))
        "unrendered only under quote")
    (is (= [false true false false] (mapv hiccup/inside-style-decl? [quoted styled required rendered]))
        "style declaration only under defattrs")
    (is (= [false false true false] (mapv hiccup/inside-ns-form? [quoted styled required rendered]))
        "ns only under ns")
    (is (= [true true true false] (mapv hiccup/inside-non-hiccup-form? [quoted styled required rendered]))
        "non-hiccup under any of three")))

(deftest enclosing-forms-remembers-ancestors-in-file-memo
  (binding [parser/*file-memo* (java.util.HashMap.)]
    (let [root (z/of-string "(defclass c [] [:a [:b] [:i]])")
          find-loc (fn [target] (z/find root z/next #(= target (z/string %))))]
      (is (= #{:style-decl} (hiccup/enclosing-forms (find-loc "[:b]")))
          "first ask walks to root")
      (let [^java.util.Map table (.get ^java.util.Map parser/*file-memo* :cljs-patrol.hiccup/enclosing-forms)
            outer (z/node (find-loc "[:a [:b] [:i]]"))]
        (is (= #{:style-decl} (.get table outer))
            "answer kept for enclosing vector")
        (is (= #{:style-decl} (.get table (z/node root)))
            "answer kept for style declaration itself")
        (.put table outer #{:ns})
        (is (= #{:ns} (hiccup/enclosing-forms (find-loc "[:i]")))
            "later ask stops at remembered ancestor")))))

(defn- result-strings
  ([source] (mapv z/string (hiccup/result-locs (z/of-string source))))
  ([ns-info source] (mapv z/string (hiccup/result-locs (z/of-string source) ns-info))))

(deftest result-locs-reads-heads-by-name-without-ns-info
  (is (= ["[:a]" "[:b]"] (result-strings "(if x [:a] [:b])"))
      "reads core if")
  (is (= ["[:a]"] (result-strings "(x/when c [:a])"))
      "reads look-alike by its bare name")
  (is (= ["[:a]" "[:b]"] (result-strings "(case k :x [:a] [:b])"))
      "reads case default"))

(deftest result-locs-resolves-heads-with-ns-info
  (is (= ["[:a]" "[:b]"] (result-strings {} "(cond x [:a] :else [:b])"))
      "reads core cond")
  (is (= ["[:a]"] (result-strings {:aliases {"r" "reagent.core"}} "(r/with-let [a 1] [:a])"))
      "reads reagent with-let through alias")
  (is (= ["(x/when c [:a])"] (result-strings {} "(x/when c [:a])"))
      "answers for look-alike itself")
  (is (= ["(when c [:a])"] (result-strings {:shadowed-core #{"when"}} "(when c [:a])"))
      "answers for head ns form shadows")
  (is (= ["(x/do [:a])" "[:c]"] (result-strings {} "(if p (x/do [:a]) [:c])"))
      "keeps unresolved branch whole"))

(defn- clear-data-source? [source] (hiccup/clear-data? (z/of-string source)))

(deftest clear-data?-reads-malli-schemas
  (is (clear-data-source? "[:map {:closed true} [:label {:optional true} [:maybe [:or :string :int]]]]")
      "map root with entry properties")
  (is (clear-data-source? "[:map [:a :int] [\"b\" :string] [1 [:vector :int]] [true :any] [nil :any]]")
      "map entries without properties and literal keys")
  (is (clear-data-source? "[:=> [:cat :int] :any]")
      "function root")
  (is (clear-data-source? "[:enum :a \"b\" \"c\nd\" 1 1.5 true sym]")
      "enum root with scalar schemas")
  (is (clear-data-source? "[:re #\"x\"]")
      "regex child")
  (is (clear-data-source? "[:vector item-schema]")
      "symbol child")
  (is (clear-data-source? "[:vector (m/schema :int)]")
      "call child")
  (is (clear-data-source? "[:multi {:dispatch :type} [:a [:map [:x :int]]] [::m/default :any]]")
      "multi entries")
  (is (clear-data-source? "[:catn [:a :int] [:b {:x 1} :string]]")
      "catn entries")
  (is (clear-data-source? "[:vector {:gen/schema [:enum :a] :gen/elements [:a :b]} :int]")
      "property values that are clearly data")
  (is (clear-data-source? "[:map {:min 1} [:a {:default [:x :y]} :int]]")
      "entry property value that is clearly data")
  (is (clear-data-source? "[:vector {:a 1 [:div \"x\"] 2} :int]")
      "property key that is no data")
  (is (clear-data-source? "[:map]")
      "map without children")
  (is (clear-data-source? "[:malli.core/schema :int]")
      "written-out malli.core schema")
  (doseq [type [":and" ":or" ":not" ":map-of" ":vector" ":sequential" ":seqable" ":every" ":set" ":maybe" ":tuple"
                ":re" ":fn" ":ref" ":=>" ":->" ":function" ":schema" ":malli.core/schema" ":any" ":some" ":nil"
                ":string" ":int" ":float" ":double" ":boolean" ":keyword" ":qualified-keyword" ":qualified-symbol"
                ":uuid" ":?" ":repeat" ":cat" ":alt"]]
    (is (clear-data-source? (str "[" type " :int]"))
        (str "data type " type))
    (is (not (clear-data-source? (str "[" type " \"x\"]")))
        (str "string child of " type)))
  (doseq [type [":enum" ":=" ":not=" ":<" ":<=" ":>="]]
    (is (clear-data-source? (str "[" type " \"x\" 1 true :a]"))
        (str "literal values of " type))
    (is (not (clear-data-source? (str "[" type " (f)]")))
        (str "call child of " type))))

(deftest clear-data?-reads-entry-schemas
  (doseq [type [":map" ":multi" ":catn" ":altn" ":orn" ":andn"]]
    (is (clear-data-source? (str "[" type " [:a :int]]"))
        (str "entry of " type))
    (is (not (clear-data-source? (str "[" type " (m/schema :int)]")))
        (str "schema in place of entry of " type))
    (is (not (clear-data-source? (str "[" type " [:a \"x\"]]")))
        (str "literal in entry schema of " type))
    (is (not (clear-data-source? (str "[" type " [:a {:optional true} 1]]")))
        (str "literal in entry schema with properties of " type))))

(deftest clear-data?-reads-property-values
  (is (clear-data-source? "[:vector {:a {:b [:enum :x]} :c #{[:settings :style]} :d ([:map]) :e #?(:cljs [:int :a])} :int]")
      "clear data nested in map, set, list and reader conditional")
  (is (not (clear-data-source? "[:vector {:a {:b [:img {:src \"a.png\"}]}} :int]"))
      "element nested in map")
  (is (not (clear-data-source? "[:vector {:a #{[:img {:src \"a.png\"}]}} :int]"))
      "element nested in set")
  (is (not (clear-data-source? "[:vector {:a (f [:img {:src \"a.png\"}])} :int]"))
      "element nested in list")
  (is (not (clear-data-source? "[:vector {:a #?(:cljs [:img {:src \"a.png\"}])} :int]"))
      "element nested in reader conditional")
  (is (not (clear-data-source? "[:vector {:a ^:x {:b [:img {:src \"a.png\"}]}} :int]"))
      "element nested in map with metadata")
  (is (clear-data-source? "[:vector {:a ^{:doc [:div \"x\"]} [:enum :b]} :int]")
      "metadata of property value looked through"))

(deftest clear-data?-reads-keyword-vectors
  (is (clear-data-source? "[:settings :style]")
      "db path")
  (is (clear-data-source? "[:line :rect :circle]")
      "keyword list of tag names")
  (is (clear-data-source? "[::local :ns/name]")
      "qualified keywords"))

(deftest clear-data?-reads-lists
  (is (clear-data-source? "[[:map [:a :int]] [:enum :x :y] [:settings :style]]")
      "list of schemas and paths")
  (is (clear-data-source? "[^:x [:map] [:enum :a]]")
      "metadata on head vector")
  (is (clear-data-source? "[[[:settings :style]] [[:map]]]")
      "nested lists"))

(deftest clear-data?-looks-through-metadata-and-discards
  (is (clear-data-source? "^{:doc \"x\"} [:map]")
      "map metadata")
  (is (clear-data-source? "^:private ^:const [:settings :style]")
      "stacked metadata")
  (is (clear-data-source? "[:map ^:x [:a ^:y [:vector ^:z [:enum :b]]]]")
      "inner metadata")
  (is (clear-data-source? "[:vector {:gen/schema ^:x [:enum :a]} :int]")
      "metadata on property value")
  (is (clear-data-source? "[#_\"x\" :settings :style]")
      "discarded head")
  (is (clear-data-source? "[:settings #_0 :style]")
      "discarded number")
  (is (clear-data-source? "[:map #_[:div \"x\"] [:a #_\"x\" :int]]")
      "discarded child and entry element")
  (is (clear-data-source? "[:vector {:a 1 #_#_:b [:div \"x\"]} :int]")
      "discarded property")
  (is (clear-data-source? "[[:enum :a] #_[:div \"x\"]]")
      "discarded element of list")
  (is (not (clear-data-source? "[:div #_:a]"))
      "single keyword left after discard"))

(deftest clear-data?-leaves-hiccup
  (is (not (clear-data-source? "[:div \"a\"]"))
      "html element")
  (is (not (clear-data-source? "[:map {:name \"nav\"} [:a {:href \"/\"} [:img {:src \"a.png\"}]]]"))
      "image map")
  (is (not (clear-data-source? "[:map {:name \"nav\"} [:a {:href \"/\"} \"Home\"]]"))
      "image map with text link")
  (is (not (clear-data-source? "[:vector [:img {:src \"a.png\"}]]"))
      "element in schema position")
  (is (not (clear-data-source? "[:map [:a [:img {:src \"a.png\"}]]]"))
      "element in entry schema position")
  (is (not (clear-data-source? "[:map {:description [:img {:src \"a.png\"}]} [:a :int]]"))
      "element in property value")
  (is (not (clear-data-source? "[:map [:a {:description [:img {:src \"a.png\"}]} :int]]"))
      "element in entry property value")
  (is (not (clear-data-source? "[:vector {:description []} :int]"))
      "empty vector in property value")
  (is (not (clear-data-source? "[:my-dialog [:button {:aria-hidden true :tab-index 0} \"x\"]]"))
      "custom element over markup")
  (is (not (clear-data-source? "[:<> {} [:p \"a\"]]"))
      "fragment with empty props")
  (is (not (clear-data-source? "[:button [icons/close]]"))
      "component without arguments")
  (is (not (clear-data-source? "[:img logo-attrs]"))
      "props named by symbol")
  (is (not (clear-data-source? "[[:li \"a\"] [:li \"b\"]]"))
      "list of elements")
  (is (not (clear-data-source? "[[:th :name] [:th :email]]"))
      "list of keyword-only elements with tag heads")
  (is (not (clear-data-source? "[[:th.cell :name] [:th#id.cell :email]]"))
      "list of keyword-only elements with class shorthand")
  (is (not (clear-data-source? "[[:settings :a] [[:th :name]]]"))
      "nested list of keyword-only element")
  (is (not (clear-data-source? "[[:enum :a] [:li \"b\"]]"))
      "list with one element")
  (is (not (clear-data-source? "[[:enum :a] :b]"))
      "list with keyword")
  (is (not (clear-data-source? "[:div.paragraph [:br {:margin-top \"5px\"}]]"))
      "garden rule")
  (is (not (clear-data-source? "[:* {:box-sizing \"border-box\"}]"))
      "garden rule headed by :*")
  (is (not (clear-data-source? "[:+ {:margin-top 0}]"))
      "garden rule headed by :+")
  (is (not (clear-data-source? "[:title \"asc\"]"))
      "pair with string")
  (is (not (clear-data-source? "[:items 0 :name]"))
      "path with number")
  (is (not (clear-data-source? "[:br]"))
      "single keyword")
  (is (not (clear-data-source? "[:symbol {:viewBox \"0 0 1 1\"}]"))
      "malli type keyword that is html tag")
  (is (not (clear-data-source? "[:> Button \"Save\"]"))
      "reagent interop head")
  (is (not (clear-data-source? "[::m/schema [:map [:a :int]]]"))
      "aliased malli.core schema head")
  (is (not (clear-data-source? "[:vector [^:x :map :int]]"))
      "keyword head with metadata"))

(deftest clear-data?-reads-svg-set-as-data
  (is (clear-data-source? "[:set {:attributeName \"fill\" :to \"red\"}]")
      "svg set element"))

(deftest clear-data?-leaves-schemas-with-other-parts
  (is (not (clear-data-source? "[:enum nil]"))
      "nil child")
  (is (not (clear-data-source? "[:enum \\a]"))
      "character child")
  (is (not (clear-data-source? "[:enum ^:x a]"))
      "literal with metadata")
  (is (not (clear-data-source? "[:vector #(f %)]"))
      "function literal child")
  (is (not (clear-data-source? "[:map :a [:b :int]]"))
      "keyword where entry belongs")
  (is (not (clear-data-source? "[:map [k :int]]"))
      "symbol key")
  (is (not (clear-data-source? "[:map [[:a] :int]]"))
      "vector key")
  (is (not (clear-data-source? "[:map [:a]]"))
      "entry without schema")
  (is (not (clear-data-source? "[:map [:a {:optional true}]]"))
      "entry with properties and no schema")
  (is (not (clear-data-source? "[:map [:a :int :string]]"))
      "entry with extra element")
  (is (not (clear-data-source? "[:map [:a {:optional true} :int :string]]"))
      "entry with properties and extra element"))

(deftest clear-data?-leaves-other-forms
  (is (false? (clear-data-source? "[]"))
      "false for empty vector")
  (is (false? (clear-data-source? "[:br]"))
      "false for single keyword")
  (is (not (clear-data-source? "[[] [:a :b]]"))
      "empty head vector")
  (is (not (clear-data-source? "(:map :a)"))
      "list")
  (is (not (clear-data-source? "{:a :b}"))
      "map")
  (is (not (clear-data-source? "[\"a\" :b]"))
      "string head"))

(defn- clear-data-def-source? [source] (hiccup/clear-data-def? (z/of-string source)))

(deftest clear-data-def?-reads-value
  (is (clear-data-def-source? "(def path [:settings :style])")
      "def")
  (is (clear-data-def-source? "(defonce path [:settings :style])")
      "defonce")
  (is (clear-data-def-source? "(def ^:private ^:const schema [:map [:a :int]])")
      "metadata on name")
  (is (clear-data-def-source? "(def schema \"Describes a tag.\" [:map [:a :int]])")
      "docstring")
  (is (clear-data-def-source? "(def schema [:map [:a :int]] #_[:div \"x\"])")
      "discarded form after value")
  (is (clear-data-def-source? "(def schema [:map ^:x [:a :int]])")
      "inner metadata"))

(deftest clear-data-def?-leaves-other-values
  (is (not (clear-data-def-source? "(def path ^:const [:settings :style])"))
      "value with own metadata")
  (is (not (clear-data-def-source? "(def text [:div \"x\"])"))
      "hiccup value")
  (is (not (clear-data-def-source? "(def paths {:a [:settings :style]})"))
      "map value")
  (is (not (clear-data-def-source? "(def schema (mu/closed-schema [:map [:a :int]]))"))
      "call value")
  (is (not (clear-data-def-source? "(def row [item-row :a :b])"))
      "symbol-headed vector value")
  (is (not (clear-data-def-source? "(def path #?(:cljs [:settings :style] :clj [:a :b]))"))
      "reader conditional value")
  (is (not (clear-data-def-source? "(def path)"))
      "no value")
  (is (not (clear-data-def-source? "(def)"))
      "no name")
  (is (not (clear-data-def-source? "(def path #_[:settings :style])"))
      "only discarded value"))

(deftest clear-data-def?-reads-top-level-defs-only
  (is (not (clear-data-def-source? "(defn path [] [:settings :style])"))
      "defn")
  (is (not (clear-data-def-source? "[def path [:settings :style]]"))
      "vector starting with def")
  (is (not (hiccup/clear-data-def? (loc-at "(do (def path [:settings :style]))" "(def path [:settings :style])")))
      "nested def")
  (is (not (hiccup/clear-data-def? (loc-at "#?(:cljs (def path [:settings :style]))" "(def path [:settings :style])")))
      "def inside reader conditional"))
