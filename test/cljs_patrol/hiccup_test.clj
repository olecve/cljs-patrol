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

  (testing "returns nil when tag name is empty after stripping"
    (is (nil? (hiccup/parse-tag ":")))
    (is (nil? (hiccup/parse-tag ":.foo")))
    (is (nil? (hiccup/parse-tag ":#bar")))))

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
