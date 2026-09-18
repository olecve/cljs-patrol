(ns cljs-patrol.hiccup-test
  (:require
   [cljs-patrol.hiccup :as hiccup]
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
      (is (= #{:src :alt} (set (keys (:attrs info)))))))

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
      (is (= #{:src :alt} (set (keys (:attrs info)))))))

  (testing "when-let and if-let bind the same way"
    (is (= :map (:kind (img-attrs-info "(when-let [props {:alt \"cat\"}] [:img props])"))))
    (is (= :map (:kind (img-attrs-info "(if-let [props {:alt \"cat\"}] [:img props] nil)")))))

  (testing "the innermost binding wins"
    (let [info (img-attrs-info "(let [props {:src \"x\"}] (let [props {:alt \"cat\"}] [:img props]))")]
      (is (= #{:alt} (set (keys (:attrs info))))
          "the inner map answers, not the outer one")))

  (testing "resolves past an enclosing form that binds nothing of that name"
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (fn [] [:img props]))")))
        "a form-2 component closes over the binding")
    (is (= :map (:kind (img-attrs-info "(let [props {:alt \"cat\"}] (for [item items] [:img props]))")))
        "for binds item, not props"))

  (testing "a bound map-building call is read like the same call written in the slot"
    (let [info (img-attrs-info "(let [props (merge base {:alt \"cat\"})] [:img props])")]
      (is (= :dynamic-map (:kind info)))
      (is (= #{:alt} (set (keys (:attrs info))))
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
    (is (= :map (:kind (img-attrs-info "[:img (assoc-in {:src \"a\"} [:alt] \"cat\")]"))))))

(deftest attrs-info-def-test
  (testing "a def in the same file names a map"
    (let [info (img-attrs-info "(def props {:src \"a\" :alt \"cat\"}) (defn v [] [:img props])")]
      (is (= :map (:kind info)))
      (is (= #{:src :alt} (set (keys (:attrs info))))))
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

  (testing "a def-shaped macro we do not know still binds its parameters"
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defnc row [props] [:img props])")))
        "a defnc parameter shadows the var of the same name")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (rum/defc row < rum/static [props] [:img props])")))
        "the name is read past the namespace and past the mixin")
    (is (= :non-map (:kind (img-attrs-info "(def props {:alt \"cat\"}) (defui App [this props] [:img props])")))
        "an unknown macro leaves the symbol unknown rather than letting the var answer"))

  (testing "the last def of a name wins, the way a re-def rebinds the var"
    (let [info (img-attrs-info "(def props {:alt \"cat\"}) (def props {:src \"a\"}) (defn v [] [:img props])")]
      (is (= #{:src} (set (keys (:attrs info)))))))

  (testing "metadata stacks in front of the name"
    (is (= :map (:kind (img-attrs-info "(def ^:private ^:const props {:alt \"cat\"}) (defn v [] [:img props])")))
        "two metadata layers")
    (is (= :map (:kind (img-attrs-info "(def ^{:doc \"d\"} ^:private props {:alt \"cat\"}) (defn v [] [:img props])")))
        "a metadata map and a shorthand layer"))

  (testing "a def of something other than a map literal answers nothing"
    (is (= :non-map (:kind (img-attrs-info "(def props (make-props)) (defn v [] [:img props])")))))

  (testing "a construction over a defined base is readable end to end"
    (let [info (img-attrs-info "(def base {:src \"a\"}) (defn v [] [:img (assoc base :alt \"cat\")])")]
      (is (= :map (:kind info))
          "nothing about the map is unknown, so absence can be asserted from it")
      (is (= #{:src :alt} (set (keys (:attrs info))))))))

(deftest attrs-slot-test
  (testing "a literal map occupies the attrs slot"
    (is (some? (hiccup/attrs-slot (vec-zloc "[:button {:on-click f}]")))))

  (testing "a symbol bound to a map literal occupies it too"
    (let [loc (-> (z/of-string "(let [props {:on-click f}] [:button props])") z/down z/rightmost)]
      (is (some? (hiccup/attrs-slot loc))
          "the body starts after the props symbol, so the button counts as empty")))

  (testing "nothing occupies the slot when it cannot be read"
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button]"))))
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button \"Save\"]"))))
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button (build-props)]"))))
    (is (nil? (hiccup/attrs-slot (vec-zloc "[:button props]"))))))

(deftest inside-quoted-form?-test
  (testing "true when the vector's immediate parent is a quote-family node"
    (let [zloc (-> (z/of-string "'[:img]") z/down)]
      (is (hiccup/inside-quoted-form? zloc))))

  (testing "true for syntax-quoted vectors"
    (let [zloc (-> (z/of-string "`[:img]") z/down)]
      (is (hiccup/inside-quoted-form? zloc))))

  (testing "false for a plain top-level vector"
    (is (not (hiccup/inside-quoted-form? (z/of-string "[:img]")))))

  (testing "false when parent is another vector"
    (let [zloc (-> (z/of-string "[[:img]]") z/down)]
      (is (not (hiccup/inside-quoted-form? zloc))))))
