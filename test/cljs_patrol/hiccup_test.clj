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

  (testing "a binding that is not a map literal leaves the slot as it was"
    (is (= :non-map (:kind (img-attrs-info "(let [props (build-props)] [:img props])")))
        "bound to a call")
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
        "a doseq binding shadows"))

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
