(ns cljs-patrol.groups.colors-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.colors :as colors]
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
  (doseq [text ["&#160;" "#main-content" "#12345" "#1234567" "#fffg" "#ffffffz"
                "x#fff" "#fff!" "\t#fff" "#fff\n" "white" "transparent" "var(--primary)"]]
    (is (empty? (run (pr-str text))) text)))

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

(deftest token-hints
  (let [files {"tokens.cljs" (str "(ns app.tokens) (def white \"#FFFFFF\") (def snow \"#fff\") "
                                  "(def alpha \"#abcd\") (def themed \"var(--primary)\") "
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
    (is (not= identity (baseline/issue->identity :color-literal (assoc finding :top-level "other"))))
    (is (not= identity (baseline/issue->identity :color-literal (assoc finding :literal "#FFF"))))))

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
