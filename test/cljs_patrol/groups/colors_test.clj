(ns cljs-patrol.groups.colors-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.disk :as disk]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.colors :as colors]
   [cljs-patrol.temp-dir :as temp-dir]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.test :refer [deftest is]]))

(def ^:private config {:allowed-namespaces #{'app.tokens}})

(defn- run
  ([source] (run config nil {"app.cljs" (str "(ns app)\n" source)}))
  ([config paths files]
   (get-in (temp-project/run-group (colors/make-group config paths) files) [:results :color-literal])))

(deftest color-shapes
  (doseq [text ["#fff" "#abcd" "#123456" "#12345678" "#ABC" "#ABCD" "#ABCDEF" "#ABCDEF12"
                "1px solid #ddd" "0 0 2px #12345678" "rgb(1,2,3)" "rgba(1,2,3,0.4)"
                "hsl(0,0%,0%)" "hsla(0,0%,0%,0.4)" "RGB(1,2,3)" "RGBA(1,2,3,0.4)"
                "HSL(0,0%,0%)" "HSLA(0,0%,0%,0.4)" "prefix rgb(1,2,3) suffix"
                "(#fff)" ",#fff," "#fff;" " #fff " "url(#beef)"]]
    (is (= 1 (count (run (pr-str text)))) text))
  (is (= ["#ABC" "#123456"]
         (mapv :literal (run "\"linear-gradient(#ABC,#123456)\"")))))

(deftest non-color-boundaries
  (doseq [text ["&#160;" "#/settings" "#12345" "#1234567" "#fffg" "#ffffffz"
                "x#fff" "#fff!" "\t#fff" "#fff\n" "white" "transparent" "var(--example-accent)"]]
    (is (empty? (run (pr-str text))) text)))

(deftest discarded-forms-do-not-set-docstring-positions
  (is (= ["#fff"] (mapv :literal (run "(def color \"#fff\" #_unused)"))))
  (is (empty? (run "(defn tint #_\"old docs\" \"#abc\" [] nil)"))))

(deftest trailing-attribute-map-documentation
  (doseq [source ["(defn tint ([x] x) {:doc \"#fff\"})"
                  "(defn tint {:private true} ([x] x) {:doc \"#fff\"})"
                  "(defn tint \"Color.\" {:private true} ([x] x) ([x y] y) {:doc \"#fff\"})"]]
    (is (empty? (run source)) source)))

(deftest reader-conditional-documentation
  (doseq [source ["(defn tint #?(:cljs \"#fff\" :clj \"#abc\") [x] x)"
                  "(defn tint #?@(:cljs [\"#fff\"] :clj (\"#abc\")) [x] x)"
                  "(defn tint #?@(:cljs #?(:cljs [\"#fff\"])) [x] x)"
                  "(defn tint #?@(:cljs ^:example [\"#fff\"]) [x] x)"
                  "(do (defn tint #?(:example \"#fff\" :cljs \"#abc\") [x] x))"
                  "(defn tint {:doc #?(:cljs \"#fff\" :default \"#abc\")} [x] x)"
                  "(defn tint #?(:cljs {:doc \"#fff\"}) [x] x)"
                  "(defn tint \"Mix #fff with white.\" #?(:cljs ([x] x) :clj ([x] x)))"
                  "(defn tint \"Mix #fff with white.\" #?@(:cljs [[x] x] :clj [[x] x]))"
                  "(def tint \"Use #fff for white.\" #?(:cljs 1 :clj 2))"
                  "(do #?(:example 1) (def tint \"Use #fff for white.\" #?(:cljs 1 :clj 2)))"
                  "(defn tint #?(:example #?(:cljs \"#fff\")) [x] x)"
                  "(#?(:cljs defn :default defn) tint \"#fff\" [] nil)"
                  "(#?(:cljs comment :default comment) \"#fff\")"
                  "#?(:default nil :cljs (comment \"#fff\"))"
                  "(def ^#?(:cljs {:doc \"#fff\"} :clj {:doc \"#abc\"}) tint 1)"
                  "(defn tint #?(:cljs \"Client.\" :clj \"Server.\" :default \"#fff\") [x] x)"
                  "(def ^#?(:cljs {} :clj {} :default {:doc \"#fff\"}) tint 1)"
                  "(defn tint ([x] x) #?(:cljs {:doc \"#fff\"}))"
                  "(def ^{:doc #?(:cljs \"#fff\")} tint 1)"
                  "(defprotocol Tint (tint [x] #?(:cljs \"#fff\" :clj \"#abc\")))"]]
    (is (empty? (run config nil {"app.cljc" source})) source)))

(deftest reader-conditional-code-literals
  (doseq [source ["(def tint #?(:cljs \"#fff\" :clj nil))"
                  "(def tint \"#fff\" #?(:clj nil))"
                  "(#?(:cljs identity :clj comment) \"#fff\")"
                  "(def ^#?(:cljs {:doc \"#abc\" :example \"#fff\"}) tint 1)"
                  "(defn tint [x] #?(:cljs {:doc \"#fff\"}))"
                  "(defn tint [x] #?@(:cljs [\"#fff\"]))"
                  "(def tint {:doc #?(:cljs \"#fff\")})"
                  "(defn tint #?(:cljs {:doc \"#abc\" :example \"#fff\"}) [x] x)"]]
    (is (= ["#fff"] (mapv :literal (run config nil {"app.cljc" source}))) source)))

(deftest reader-conditional-baseline-identities
  (let [findings (run config nil
                      {"app.cljc" (str "(ns app) #?(:cljs (def first-color \"#fff\")) "
                                       "#?(:cljs (def second-color \"#fff\"))")})
        identities (mapv #(baseline/issue->identity :color-literal %) findings)]
    (is (= ["first-color" "second-color"] (mapv :name identities)))
    (is (= 2 (count (set identities)))))
  (is (= ["nested-color"]
         (mapv :top-level
               (run config nil {"app.cljc" "(ns app) #?(:cljs (def nested-color (identity \"#fff\")))"})))))

(deftest reader-conditional-definition-names-distinguish-baseline-identities
  (let [source (str "(ns app) (def #?(:cljs first-color :default default-color) \"#fff\") "
                    "(def #?(:cljs second-color :default fallback-color) \"#fff\") "
                    "#?(:clj (def #?(:clj only-clj :default other) \"#fff\")) "
                    "#?(:cljs nil :clj nil :default (def #?(:cljs other :default portable) \"#fff\"))")
        findings (run config nil {"app.cljc" source})
        identities (map #(baseline/issue->identity :color-literal %) findings)]
    (is (= ["first-color" "second-color" "only-clj" "portable"] (mapv :name identities)))
    (is (= 4 (count (set identities))))))

(deftest all-expression-positions
  (doseq [source ["(def warning \"#fff\")" "[:circle {:fill \"#fff\"}]"
                  "(set! (.-fillStyle ctx) \"#fff\")" "{:series [{:color \"#fff\"}]}"
                  "(defclass card [] {:color \"#fff\"})"
                  "(defclass row [] [:&:hover {:color \"#fff\"}])"
                  "(defn tint [] {:doc \"#fff\"})"
                  "(def chart {:doc \"#fff\"})" "(defonce chart {:doc \"#fff\"})"
                  "^:data {:doc \"#fff\"}"
                  "(foo x {:doc \"#fff\"} 1)" "[def x {:doc \"#fff\"} 1]"
                  "(foo x \"#fff\" 1)" "[def x \"#fff\" 1]"
                  "(defonce warning \"#fff\")" "(def ^:private warning \"#fff\")"
                  "(defn warning [] \"#fff\")" "(str \"text\n #fff\")"]]
    (is (= 1 (count (run source))) source)))

(deftest skipped-forms
  (doseq [source ["; #fff\n" "(comment (draw! \"#fff\"))" "#_(draw! \"#fff\")"
                  "(ns app \"#fff\")" "(defn tint \"#fff\" [x] x)"
                  "(def ^:private tint \"#fff\" 1)" "(def tint \"#fff\" 1)"
                  "(defn tint \"#fff\n#abc\" [x] x)"
                  "(defprotocol Tint \"#fff\" (tint [x] \"#abc\"))"
                  "(defprotocol Tint (tint [x]))"
                  "(defn tint {:doc \"#fff\"} [x] x)"
                  "(defn tint \"Color.\" {:doc \"#fff\"} [x] x)"
                  "(def ^{:doc \"#fff\"} tint 1)"]]
    (is (empty? (run source)) source))
  (is (empty? (run config nil {"app.cljs" "(ns app.tokens) (def white \"#fff\")"})))
  (is (empty? (run config nil {"app.cljs" "(ns app-test) (def white \"#fff\")"})))
  (is (empty? (run config [] {"app.cljs" "(ns app-test) (def white \"#fff\")"})))
  (is (empty? (run config ["checks"] {"checks/app.cljs" "(ns app) (def white \"#fff\")"})))
  (is (= 1 (count (run config ["checks"] {"app.cljs" "(ns app-test) \"#fff\""}))))
  (is (= 1 (count (run config ["test"] {"latest/app.cljs" "(ns app) \"#fff\""}))))
  (is (empty? (run config nil {"app.clj" "(ns app) \"#fff\""})))
  (is (= 1 (count (run config nil {"app.cljc" "(ns app) \"#fff\""}))))
  (is (= 1 (count (run config nil {"app.cljs" "\"#fff\""})))))

(deftest single-test-path-matches-whole-segments
  (is (= ["#123"]
         (mapv :literal
               (run config "test"
                    {"test/app.cljs" "(ns app.spec) \"#fff\""
                     "src/t/app.cljs" "(ns app.t) \"#123\""})))))

(deftest quoted-definitions-do-not-supply-token-hints
  (is (= ["Use app.tokens/white."]
         (mapv :hint
               (run config nil
                    {"tokens.cljs" (str "(ns app.tokens) (def white \"#fff\") "
                                        "(def example '(def literal \"#fff\")) "
                                        "(def other (quote (def explicit \"#fff\"))) "
                                        "(def template `(def syntax \"#fff\")) "
                                        "(def nested-template `[`[~(def nested \"#fff\")]])")
                     "app.cljs" "(ns app) \"#fff\""})))))

(deftest unquoted-definitions-supply-token-hints
  (is (= ["Use app.tokens/black, app.tokens/gray, app.tokens/ink, app.tokens/paper, app.tokens/white."]
         (mapv :hint
               (run config nil
                    {"tokens.cljs" (str "(ns app.tokens) "
                                        "`[~(def white \"#fff\") ~@(do (def gray \"#fff\") []) "
                                        "'~(def black \"#fff\") "
                                        "(#_old clojure.core/unquote (def ink \"#fff\")) "
                                        "(clojure.core/unquote-splicing (do (def paper \"#fff\") [])) "
                                        "(example/unquote (def phantom \"#fff\"))]")
                     "app.cljs" "(ns app) \"#fff\""})))))

(deftest token-hints
  (let [files {"tokens.cljs" (str "(ns app.tokens) (def white \"#FFFFFF\") (def snow \"#fff\") "
                                  "(def alpha \"#abcd\") (def themed \"var(--example-accent)\") "
                                  "(def border \"1px solid #123\") (def other 1) (defn fake [] \"#123\")")
               "app.cljs" "(ns app) [\"#fff\" \"#AABBCCDD\" \"#ffff\" \"#123\" \"rgb(0,0,0)\"]"}
        findings (run config nil files)]
    (is (= ["Use app.tokens/snow, app.tokens/white." "Use app.tokens/alpha."
            "Use a token from app.tokens." "Use a token from app.tokens." "Use a token from app.tokens."]
           (mapv :hint findings)))
    (is (= ["Use a token from app.tokens."]
           (mapv :hint (run config nil {"app.cljs" "(ns app) \"#fff\""}))))))

(deftest tokens-in-shared-files-and-hint-order
  (is (= ["Use app.tokens.extra/white, app.tokens/white."]
         (mapv :hint
               (run {:allowed-namespaces #{'app.tokens 'app.tokens.extra}} nil
                    {"tokens.cljc" "(ns app.tokens) #?(:cljs (def ^:private white \"#fff\"))"
                     "extra.cljs" "(ns app.tokens.extra) (def white \"#FFFFFF\")"
                     "app.cljs" "(ns app) \"#fff\""}))))
  (is (= ["Use a token from app.tokens."]
         (mapv :hint
               (run config nil
                    {"tokens.cljs" (str "(ns app.tokens) #_(def discarded \"#fff\") "
                                        "(comment (def example \"#fff\")) (def documented \"#fff\" 1)")
                     "app.cljs" "(ns app) \"#fff\""})))))

(deftest reader-conditional-token-hints
  (let [files {"tokens.cljc" (str "(ns app.tokens) "
                                  "#?(:clj (def clj-only \"#fff\")) "
                                  "#?(:clj (def shared \"#fff\") :cljs (def shared \"#abc\")) "
                                  "(def tint #?(:clj \"#fff\" :cljs \"#123\")) "
                                  "(do #?@(:clj [(def absent \"#fff\")] :cljs [(def spliced \"#456\")])) "
                                  "#?(:default (def first-match \"#789\") :cljs (def unreachable \"#fff\")) "
                                  "#?(:cljs (def cljs-first \"#def\") :default (def late-default \"#fff\")) "
                                  "#?(:cljs #?(:clj (def nested-clj \"#fff\") :cljs (def nested \"#999\"))) "
                                  "(#?(:cljs def :clj defonce) headed \"#fed\") "
                                  "(#?(:cljs comment :clj identity) (def hidden \"#fff\"))")
               "app.cljs" "(ns app) [\"#fff\" \"#abc\" \"#123\" \"#456\" \"#789\" \"#def\" \"#999\" \"#fed\"]"}]
    (is (= ["Use a token from app.tokens." "Use app.tokens/shared." "Use app.tokens/tint."
            "Use app.tokens/spliced." "Use app.tokens/first-match." "Use app.tokens/cljs-first."
            "Use app.tokens/nested." "Use app.tokens/headed."]
           (mapv :hint (run config nil files))))))

(deftest config-validation
  (is (empty? (run nil nil {"app.cljs" "\"#fff\""})))
  (doseq [value [false "tokens" {} {:allowed-namespaces ['app.tokens]}
                 {:allowed-namespaces #{"app.tokens"}} {:allowed-namespaces #{'app/tokens}}
                 {:allowed-namespaces nil}
                 {:allowed-namespaces #{}
                  :extra true}]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":colors :allowed-namespaces"
                          (colors/make-group value)) (pr-str value)))
  (is (= ["Use a token from an allowed namespace."]
         (mapv :hint (run {:allowed-namespaces #{}} nil {"app.cljs" "\"#fff\""})))))

(deftest finding-fields-and-baseline
  (let [finding (first (run "(def white \"#fff\")"))
        identity (baseline/issue->identity :color-literal finding)]
    (is (= {:name "white"
            :literal "#fff"
            :rule :color-literal}
           (dissoc identity :file)))
    (is (= {:row 2
            :col 12
            :top-level "white"
            :literal "#fff"
            :form "\"#fff\""
            :kw :color-literal}
           (select-keys finding [:row :col :top-level :literal :form :kw])))
    (is (= identity (baseline/issue->identity :color-literal (assoc finding :row 200 :col 30))))
    (is (not= identity (baseline/issue->identity :color-literal (assoc finding :file "other.cljs"))))
    (is (not= identity (baseline/issue->identity :color-literal (assoc finding :top-level "other"))))
    (is (not= identity (baseline/issue->identity :color-literal (assoc finding :literal "#FFF"))))))

(deftest effective-token-declarations
  (is (= ["Use app.tokens/white."]
         (mapv :hint (run config nil
                          {"tokens.cljs" "(ns app.tokens) (def #_old white \"#fff\")"
                           "app.cljs" "(ns app) \"#fff\""}))))
  (is (= ["Use app.tokens/ink."]
         (mapv :hint (run config nil
                          {"tokens.cljs" "(ns app.tokens) ^:example (#_old def ^:private ink \"#123\")"
                           "app.cljs" "(ns app) \"#123\""})))))

(deftest metadata-wrapped-definition-identities
  (let [findings (run "^:example (def a \"#fff\") ^:example (def b \"#fff\")")
        identities (mapv #(baseline/issue->identity :color-literal %) findings)]
    (is (= ["a" "b"] (mapv :name identities)))
    (is (= 2 (count (set identities))))))

(deftest effective-documentation-map-entries
  (doseq [source ["(defn tint {:doc #_\"old\" \"#fff\"} [] nil)"
                  "(def ^{:doc #_\"old\" \"#fff\"} tint 1)"
                  "(defn tint ([x] x) {:doc #_\"old\" \"#fff\"})"
                  "(defn tint {#_:old :doc ^:example \"#fff\"} [] nil)"]]
    (is (empty? (run source)) source)))

(deftest documentation-maps-with-other-values
  (is (= ["#abc"]
         (mapv :literal (run "(defn tint {:doc \"#fff\" :example \"#abc\"} [] nil)")))))

(deftest registration-identities
  (is (= [":example/tint" nil]
         (mapv :top-level (run "(register #_old :example/tint \"#fff\") (render value \"#fff\")")))))

(deftest effective-list-heads
  (doseq [source ["(#_old comment \"#fff\")"
                  "(#_old defn tint \"#fff\" [] nil)"
                  "^:example (defn tint \"#fff\" [] nil)"
                  "(^:example comment \"#fff\")"
                  "(#_old defprotocol Tint (#_old tint [x] \"#fff\"))"]]
    (is (empty? (run source)) source)))

(deftest metadata-does-not-hide-code-literals
  (is (= ["#123" "#456"]
         (mapv :literal (run "(def ^{:doc \"#abc\" :example \"#123\"} tint ^:example [comment \"#456\"])"))))
  (is (empty? (run "(defn tint ^:example {:doc \"#123\"} [] nil)"))))

(deftest only-lists-supply-definition-identities
  (is (= [nil "tint"]
         (mapv :top-level
               (run config nil
                    {"app.cljc" "(ns app) [def color \"#fff\"] ^:example #?(:cljs (def tint \"#fff\"))"})))))

(deftest only-named-def-initializers-supply-token-hints
  (is (= ["Use a token from app.tokens." "Use app.tokens/white."]
         (mapv :hint (run config nil
                          {"tokens.cljs" "(ns app.tokens) (def :malformed \"#123\") (def white \"#abc\" ^:example \"#fff\")"
                           "app.cljs" "(ns app) [\"#123\" \"#fff\"]"})))))

(deftest token-hints-across-source-roots
  (temp-dir/with-dir "cljs-patrol-colors-roots"
    (fn [dir]
      (let [app (disk/path dir "app")
            tokens (disk/path dir "tokens")]
        (disk/write! (disk/path app "core.cljs") "(ns app) (def tint \"#fff\")")
        (disk/write! (disk/path tokens "core.cljs") "(ns app.tokens) (def white \"#fff\")")
        (let [results (core/run-all [(disk/typed-path app) (disk/typed-path tokens)] [(colors/make-group config)])]
          (is (= [["Use app.tokens/white."] []]
                 (mapv #(mapv :hint (get-in % [:group-results 0 :color-literal])) results))))))))

(deftest group-contract
  (let [rule-group (colors/make-group config)]
    (is (= :colors (group/group-id rule-group)))
    (is (= "Colors" (group/group-name rule-group)))
    (is (= #{".cljs" ".cljc"} (group/file-extensions rule-group)))
    (is (= {:color-literal :cleanup} (group/rule->tier rule-group)))
    (is (= {:color-literal "color-literal"} (group/docs-anchors rule-group)))
    (is (= {:color-literal "A color literal sits outside the token namespaces. Use a design token."}
           (group/suggestions rule-group)))
    (is (false? (group/failed? rule-group {:color-literal [{}]})))
    (is (= [["Color literals:" 2]] (group/summary-lines rule-group {:color-literal [{} {}]})))
    (is (nil? (group/summary-lines (colors/make-group) {})))))
