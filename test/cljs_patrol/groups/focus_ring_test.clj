(ns cljs-patrol.groups.focus-ring-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.focus-ring :as focus-ring]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.test :refer [deftest is]]))

(def ^:private config
  {:approved-shadows {'app.styles.focus-rings/primary-shadow #{'app.theme.button/shadow-brand-focused "#516bff"}}})

(def ^:private ns-form
  (str "(ns app.core (:require [spade.core :refer [defclass defattrs defglobal]]"
       " [app.styles.focus-rings :as focus-rings :refer [danger-shadow]]"
       " [app.theme.button :as button-theme]))\n"))

(defn- run
  "Analyze `source` as app/core.cljs, or as `path`, under `config`, and return the group's results."
  ([source] (run config source))
  ([config source] (run config "app/core.cljs" source))
  ([config path source]
   (:results (temp-project/run-group (focus-ring/make-group config) {path (str ns-form source)}))))

(defn- flagged
  "Return {rule [declaration names]} for every rule with findings."
  [results]
  (into {}
        (keep (fn [[rule items]]
                (when (and (sequential? items) (seq items))
                  [rule (mapv (comp name :kw) items)])))
        results))

(def ^:private ring "focus-rings/primary-shadow")

(def ^:private outline ":outline \"2px solid transparent\"")

(defn- focus-class [declaration-name selector entries]
  (str "(defclass " declaration-name " [] [" selector " {" entries "}])\n"))

(defn- visible-ring [& entries] (focus-class "a" ":&:focus-visible" (apply str entries)))

(deftest approved-ring-with-outline-passes
  (is (= {} (flagged (run (visible-ring ":box-shadow " ring " " outline))))
      "approved shadow with outline"))

(deftest ring-without-outline-is-flagged
  (let [results (run (focus-class "a" "\"&:focus-visible::after\"" (str ":box-shadow " ring)))]
    (is (= {:focus-ring-missing-outline ["a"]} (flagged results))
        "missing outline")
    (is (= "Set :outline \"2px solid transparent\" beside :box-shadow."
           (-> results :focus-ring-missing-outline first :hint))
        "hint names default outline")))

(deftest outline-that-draws-nothing-is-flagged
  (doseq [value [":none" "\"NONE\"" "0" "\"0 solid transparent\"" "\"0.0px solid red\"" "\"2px hidden\""]]
    (is (= {:focus-ring-missing-outline ["a"]} (flagged (run (visible-ring ":box-shadow " ring " :outline " value))))
        value)))

(deftest any-drawn-outline-passes-without-configured-value
  (doseq [value ["\"1px solid transparent\"" "\"2px  Solid  Transparent\"" "\"2px dotted red\"" "\"0.5px solid\""]]
    (is (= {} (flagged (run (visible-ring ":box-shadow " ring " :outline " value))))
        value)))

(deftest configured-outline-is-enforced
  (let [exact (assoc config :outline "2px solid transparent")
        results (run exact (visible-ring ":box-shadow " ring " :outline \"1px solid transparent\""))]
    (is (= {:focus-ring-missing-outline ["a"]} (flagged results))
        "other outline")
    (is (= "Set :outline \"2px solid transparent\" beside :box-shadow."
           (-> results :focus-ring-missing-outline first :hint))
        "hint names configured outline")
    (is (= {} (flagged (run exact (visible-ring ":box-shadow " ring " :outline \"2px Solid Transparent !important\""))))
        "configured outline in other case with important"))
  (is (= "Set :outline \"1px dashed transparent\" beside :box-shadow."
         (-> (run (assoc config :outline "1px dashed transparent") (visible-ring ":box-shadow " ring))
             :focus-ring-missing-outline
             first
             :hint))
      "hint names other configured outline"))

(deftest outline-check-turns-off
  (is (= {} (flagged (run (assoc config :outline false) (visible-ring ":box-shadow " ring))))
      "outline false"))

(deftest outline-read-from-symbol-is-accepted
  (is (= {} (flagged (run (visible-ring ":box-shadow " ring " :outline focus-rings/outline"))))
      "symbol outline value"))

(deftest outline-in-another-map-of-block-passes
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "} {:box-shadow " ring "}])"))))
      "two maps in one block"))

(deftest outline-in-enclosing-block-passes
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "} [:&::after {:box-shadow " ring "}]])"))))
      "outline on enclosing focus block")
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "} [:.icon {:box-shadow " ring "}]])"))))
      "outline on focus block around descendant")
  (doseq [selector [":&:hover" ":&:enabled" "\"&:not(:disabled)\"" "\"&:is(.a, .b)\""]]
    (is (= {} (flagged (run (str "(defclass a [] [" selector " {" outline "} [:&:focus-visible {:box-shadow " ring "}]])"))))
        selector)))

(deftest outline-on-another-element-is-flagged
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] [:.btn {" outline "} [:input:focus {:box-shadow " ring "}]])"))))
      "outline on ancestor without focus")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] {" outline "} [:input:focus {:box-shadow " ring "}])"))))
      "outline in base map, ring on descendant")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] [:&:hover [:.icon {" outline "}] [:&:focus-visible {:box-shadow " ring "}]])"))))
      "outline on sibling descendant"))

(deftest block-built-by-call-skips-outline
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible (merge base) {:box-shadow " ring "}])"))))
      "merge in same block")
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible (merge base) [:&::after {:box-shadow " ring "}]])"))))
      "merge in enclosing block"))

(deftest reset-is-left-alone
  (doseq [value [":none" "\"none\"" "\"unset\"" ":initial"]]
    (is (= {} (flagged (run (focus-class "a" ":textarea:focus" (str ":box-shadow " value)))))
        value)))

(deftest selectors-outside-focus-are-left-alone
  (is (= {} (flagged (run (focus-class "a" ":&:hover" ":box-shadow \"0 0 0 2px #516bff\""))))
      "hover")
  (is (= {} (flagged (run (focus-class "a" "\"&:not(:focus-visible)\"" ":box-shadow \"0 0 0 2px #516bff\""))))
      "focus inside not")
  (is (= {} (flagged (run (focus-class "a" ":focus" ":box-shadow \"0 0 0 2px #516bff\""))))
      "keyword naming element, not pseudo-class")
  (is (= {} (flagged (run "(defclass a [] {:box-shadow \"0 0 0 2px #516bff\"})")))
      "base map"))

(deftest focus-selectors-are-read-anywhere
  (doseq [selector ["\".ProseMirror:focus-visible\""
                    "\":&:focus-within:not(:has(button:focus))\""
                    ":&:focus"
                    "\"&:focus-visible::after\""]]
    (is (= {:focus-ring-missing-outline ["a"]} (flagged (run (focus-class "a" selector (str ":box-shadow " ring)))))
        selector))
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] [:&:focus-visible [:.icon {:box-shadow " ring "}]])"))))
      "nested under focus block")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] [:&:hover :&:focus-visible {:box-shadow " ring "}])"))))
      "second selector of block"))

(deftest configured-selectors-replace-defaults
  (let [only-visible (assoc config :selectors #{":focus-visible"})]
    (is (= {} (flagged (run only-visible (focus-class "a" ":&:focus" (str ":box-shadow " ring)))))
        "focus not configured")
    (is (= {:focus-ring-missing-outline ["a"]}
           (flagged (run only-visible (visible-ring ":box-shadow " ring))))
        "focus-visible configured"))
  (is (= {} (flagged (run (assoc config :selectors #{":focus"}) (visible-ring ":box-shadow " ring))))
      "focus not matched inside focus-visible"))

(deftest map-entries-are-read-past-discards-and-string-keys
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (visible-ring "\"box-shadow\" " ring))))
      "string key")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (visible-ring "#_:color :box-shadow " ring))))
      "discarded key before entries"))

(deftest declaration-kinds-are-read
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defattrs a [] [:&:focus-visible {:box-shadow " ring "}])"))))
      "defattrs")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defglobal a [\"button:focus-visible\" {:box-shadow " ring "}])"))))
      "defglobal")
  (is (= {} (flagged (run (str "(defn a [] [:&:focus-visible {:box-shadow " ring "}])"))))
      "plain function")
  (is (= {} (flagged (run "(defclass a [{:keys [x]}] {:color x})")))
      "destructuring parameter vector"))

(deftest hand-built-shadow-is-flagged
  (doseq [value ["\"0 0 0 2px var(--ring)\""
                 "button-theme/other-shadow"
                 "[[0 0 0 \"2px\" \"red\"]]"
                 "(str \"0 0 0 2px \" button-theme/ring-color)"]]
    (is (= {:focus-ring-raw-shadow ["a"]} (flagged (run (visible-ring ":box-shadow " value " " outline))))
        value)))

(deftest stacked-ring-holding-approved-shadow-passes
  (let [stacked (str ":box-shadow (str highlight \", \" " ring ") " outline)]
    (doseq [selector [":&:focus-visible" ":&:focus" ":&:focus-within"]]
      (is (= {} (flagged (run (focus-class "a" selector stacked))))
          selector)))
  (is (= {:focus-ring-raw-shadow ["a"]}
         (flagged (run (visible-ring ":box-shadow (str \"0 0 0 2px \" button-theme/ring-color) " outline))))
      "str without approved shadow"))

(deftest shadow-resolves-through-refer-and-full-name
  (let [referred (str ":box-shadow danger-shadow " outline)]
    (is (= {:focus-ring-raw-shadow ["a"]} (-> referred visible-ring run flagged))
        "referred name not approved")
    (is (= {} (flagged (run {:approved-shadows #{'app.styles.focus-rings/danger-shadow}} (visible-ring referred))))
        "referred name approved"))
  (is (= "Use app.styles.focus-rings/primary-shadow."
         (-> (run (visible-ring ":box-shadow app.theme.button/shadow-brand-focused " outline))
             :focus-ring-raw-shadow
             first
             :hint))
      "fully qualified replaced token")
  (is (= {} (flagged (run (visible-ring ":box-shadow app.styles.focus-rings/primary-shadow " outline))))
      "fully qualified approved shadow"))

(deftest unreadable-shadow-skips-source-check
  (is (= {} (flagged (run (str "(defclass a [ring] [:&:focus-visible {:box-shadow ring " outline "}])"))))
      "local")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run "(defclass a [ring] [:&:focus-visible {:box-shadow ring}])")))
      "local still needs outline")
  (is (= {} (flagged (run (visible-ring "\n:box-shadow (if highlighted? x y)"))))
      "call other than str"))

(deftest custom-property-shadow-is-hand-built
  (is (= {:focus-ring-raw-shadow ["a"]}
         (flagged (run (focus-class "a" ":&:focus" (str ":box-shadow \"var(--ring) !important\" " outline)))))
      "custom property alone"))

(defn- hint-of [config source] (-> (run config source) :focus-ring-raw-shadow first :hint))

(deftest replaced-value-names-approved-shadow
  (let [use-primary "Use app.styles.focus-rings/primary-shadow."]
    (doseq [value ["button-theme/shadow-brand-focused"
                   "(str \"inset \" button-theme/shadow-brand-focused)"
                   "\"0 0 0 2px #516BFF\""
                   "(str \"0 0 0 2px \" \"#516bff\")"
                   "\"var(--ring, 0 0 0 2px #516bff)\""
                   "(str (darken \"#516bff\"))"]]
      (is (= use-primary (hint-of config (visible-ring ":box-shadow " value " " outline)))
          value))
    (is (= use-primary (hint-of config (visible-ring ":box-shadow (str " ring " \", 0 0 0 1px #516bff\") " outline)))
        "replaced color stacked with approved shadow")
    (is (= use-primary (hint-of {:approved-shadows {'app.styles.focus-rings/primary-shadow #{"#516BFF"}}}
                                (visible-ring ":box-shadow \"0 0 0 2px #516bff\" " outline)))
        "upper-case color in config")
    (is (= use-primary (hint-of {:approved-shadows {'app.styles.focus-rings/primary-shadow #{"rgb(81, 107, 255)"}}}
                                (visible-ring ":box-shadow \"0 0 0 2px rgb(81,107,255)\" " outline)))
        "rgb written with other spacing"))
  (is (nil? (hint-of config (visible-ring ":box-shadow \"0 0 0 2px red\" " outline)))
      "no hint for other value")
  (is (nil? (hint-of {:approved-shadows {'app.styles.focus-rings/primary-shadow #{"#fff"}}}
                     (visible-ring ":box-shadow \"0 0 0 2px #fffabc\" " outline)))
      "longer color")
  (is (= {:focus-ring-raw-shadow ["a"]
          :focus-ring-missing-outline ["a"]}
         (flagged (run (visible-ring ":box-shadow \"0 0 0 2px #516bff\""))))
      "replaced color and missing outline"))

(deftest preset-namespace-checks-only-outline
  (let [preset-source (fn [entries]
                        (str "(ns app.styles.focus-rings (:require [spade.core :refer [defclass]]))\n"
                             (visible-ring entries)))
        run-preset #(:results (temp-project/run-group (focus-ring/make-group config) {"app/styles/focus_rings.cljs" %}))]
    (is (= {} (flagged (run-preset (preset-source (str ":box-shadow \"0 0 0 2px #516bff\" " outline)))))
        "hand-built ring in preset")
    (is (= {:focus-ring-missing-outline ["a"]}
           (flagged (run-preset (preset-source ":box-shadow \"0 0 0 2px #516bff\""))))
        "outline still required")))

(deftest empty-keys-turn-off-their-checks
  (let [source (visible-ring ":box-shadow \"0 0 0 2px #516bff\"")]
    (is (= {:focus-ring-raw-shadow ["a"]}
           (flagged (run {:approved-shadows #{'app.styles.focus-rings/primary-shadow}
                          :outline false}
                         source)))
        "only approved shadows")
    (is (= {:focus-ring-missing-outline ["a"]} (flagged (run {} source)))
        "empty config checks only outline")
    (is (= {} (flagged (run {:outline false} source)))
        "outline off")))

(deftest unconfigured-group-does-nothing
  (is (= {} (group/parse-handlers focus-ring/group))
      "no handlers")
  (is (empty? (group/summary-lines focus-ring/group {}))
      "no summary lines")
  (is (= 2 (count (group/summary-lines (focus-ring/make-group {}) {})))
      "summary lines once configured")
  (let [source (str ns-form (visible-ring ":box-shadow \"0 0 0 2px #516bff\""))]
    (is (= {} (flagged (:results (temp-project/run-group focus-ring/group {"app/core.cljs" source}))))
        "no findings")))

(deftest any-finding-fails
  (doseq [rule (keys (group/rule->tier focus-ring/group))]
    (is (group/failed? focus-ring/group {rule [{:kw :app/x}]})
        (name rule)))
  (is (not (group/failed? focus-ring/group {:focus-ring-missing-outline []}))
      "empty results"))

(deftest important-reset-is-left-alone
  (is (= {} (flagged (run (visible-ring ":box-shadow \"none !important\""))))
      "none with important"))

(deftest outline-on-decorated-element-passes
  (let [ring-after (str "[\"&:focus-visible::after\" {:box-shadow " ring "}]")]
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {:outline :none}]"
                                 " [\"&:focus-visible::after\" {:box-shadow " ring " " outline "}])"))))
        "outline on pseudo-element beside none on element")
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "}]"
                                 " [:&:focus-visible [:&::after {:box-shadow " ring "}]])"))))
        "nested pseudo-element")
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible :&:hover {" outline "}]"
                                 " [\"&:focus-visible::after\" \"&:hover::after\" {:box-shadow " ring "}])"))))
        "several selectors")
    (is (= {:focus-ring-missing-outline ["a"]}
           (flagged (run (str "(defclass a [] [:&:focus-visible {:outline :none}] " ring-after ")"))))
        "none on element and no outline on pseudo-element")
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "}] " ring-after ")"))))
        "outline on sibling block of element")
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible (merge base)] " ring-after ")"))))
        "sibling built by call")
    (is (= {:focus-ring-missing-outline ["a"]}
           (flagged (run (str "(defclass a [] [:&:hover {" outline "}] " ring-after ")"))))
        "outline on sibling with other selector")
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "}]"
                                 " [\"&:focus-visible:before\" {:box-shadow " ring "}])"))))
        "single-colon pseudo-element")))

(deftest garden-vector-is-read-like-str
  (doseq [value [(str "[highlight " ring "]") (str "[" ring "]") (str "(str " ring " \" !important\")")]]
    (is (= {} (flagged (run (focus-class "a" ":&:focus" (str ":box-shadow " value " " outline)))))
        value))
  (is (= {:focus-ring-raw-shadow ["a"]}
         (flagged (run (focus-class "a" ":&:focus" (str ":box-shadow [button-theme/ring \"red\"] " outline)))))
      "vector without approved shadow"))

(deftest str-of-unreadable-part-skips-raw-shadow
  (is (= {} (flagged (run (focus-class "a" ":&:focus" (str ":box-shadow (str (focus-rings/ring :primary)) " outline)))))
      "call inside str")
  (let [source (str "(defclass a [highlight] [:&:focus {:box-shadow (str highlight \" !important\") " outline "}])")]
    (is (= {} (flagged (run source)))
        "local inside str")))

(deftest outline-written-another-way-passes
  (is (= {} (flagged (run (focus-class "a" ":&:focus-visible"
                                       (str ":box-shadow " ring " :outline \"2px solid transparent !important\"")))))
      "outline with important")
  (is (= {} (flagged (run (focus-class "a" ":&:focus-visible"
                                       (str ":box-shadow " ring " :outline-width \"2px\" :outline-color :transparent")))))
      "outline as longhands")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (visible-ring ":box-shadow " ring " :outline-offset \"2px\""))))
      "offset alone"))

(deftest pointer-only-focus-is-left-alone
  (is (= {} (flagged (run (focus-class "a" "\"&:focus:not(:focus-visible)\"" ":box-shadow \"0 1px 2px #0003\""))))
      "focus without focus-visible")
  (is (= {} (flagged (run "(defclass a [] [:&:focus [\"&:not(:focus-visible)\" {:box-shadow \"0 1px 2px red\"}]])")))
      "nested focus without focus-visible")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (focus-class "a" "\"&:focus-within:not(:focus-visible)\"" (str ":box-shadow " ring)))))
      "focus-within still counts"))

(deftest outline-color-arguments-are-no-width
  (doseq [value ["\"2px solid rgb(0 0 0)\"" "\"2px solid hsl(0 0% 100% / 0)\"" "\"calc( 0px + 2px) solid\""]]
    (is (= {} (flagged (run (visible-ring ":box-shadow " ring " :outline " value))))
        value))
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (visible-ring ":box-shadow " ring " :outline \"0 solid rgb(0 0 0)\""))))
      "zero width outside color"))

(deftest outline-that-always-applies-passes
  (is (= {} (flagged (run (str "(defclass a [] {" outline "} [:&:focus-visible {:box-shadow " ring "}])"))))
      "outline in base map")
  (is (= {} (flagged (run (str "(defclass a [] [:.btn {" outline "} [:&:focus-visible {:box-shadow " ring "}]])"))))
      "outline on block with no state")
  (is (= {} (flagged (run (str "(defclass a [] [:& {" outline "}] [:&:focus-visible {:box-shadow " ring "}])"))))
      "outline in ampersand block")
  (is (= {} (flagged (run (str "(defclass a [] [:& {" outline "}] [:&:focus-visible::after {:box-shadow " ring "}])"))))
      "outline in ampersand block, ring on pseudo-element")
  (is (= {} (flagged (run (str "(defclass a [] [:&.active {" outline "} [:&:focus-visible {:box-shadow " ring "}]])"))))
      "outline on class qualifier")
  (is (= {} (flagged (run (str "(defclass a [] [:& :.x {" outline "}] [:&:focus-visible {:box-shadow " ring "}])"))))
      "ampersand in selector list")
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] [:.x {" outline "}] [:&:focus-visible {:box-shadow " ring "}])"))))
      "block of descendant only"))

(deftest element-block-is-found-however-spelled
  (doseq [[element ring-selector] [["\"&:focus-visible\"" ":&:focus-visible::after"]
                                   [":&:focus-visible" "\"&:focus-visible::AFTER\""]
                                   [":.btn:focus-visible" ":.btn:focus-visible::after"]
                                   [":&:focus-visible :&:focus-within"
                                    "\"&:focus-visible::after, &:focus-within::after\""]]]
    (is (= {} (flagged (run (str "(defclass a [] [" element " {" outline "}] [" ring-selector " {:box-shadow " ring "}])"))))
        ring-selector)))

(deftest configured-outline-ignores-spacing-in-color
  (is (= {} (flagged (run (assoc config :outline "2px solid rgba(0,0,0,0)")
                          (visible-ring ":box-shadow " ring " :outline \"2px solid rgba(0, 0, 0, 0)\""))))
      "rgba spacing"))

(deftest ring-on-pseudo-elements-of-several-elements
  (let [ring-after (str "[:&:focus-visible::after :&:focus-within::after {:box-shadow " ring "}]")]
    (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "}]"
                                 " [:&:focus-within {" outline "}] " ring-after ")"))))
        "every element sets outline")
    (is (= {:focus-ring-missing-outline ["a"]}
           (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "}] " ring-after ")"))))
        "one element without block")
    (is (= {:focus-ring-missing-outline ["a"]}
           (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "}]"
                              " [:&:focus-within {:outline :none}] " ring-after ")"))))
        "one element without outline")))

(deftest pointer-only-negation-keeps-focus-within
  (is (= {:focus-ring-missing-outline ["a"]}
         (flagged (run (str "(defclass a [] [:&:focus-within [\"&:not(:focus-visible)\" {:box-shadow " ring "}]])"))))
      "nested focus-within"))

(deftest malformed-approved-shadows-are-refused
  (doseq [approved [#{"app.styles.focus-rings/primary-shadow"}
                    #{'primary-shadow}
                    'app.styles.focus-rings/primary-shadow
                    {"app.styles.focus-rings/primary-shadow" #{"#516bff"}}]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":focus-ring :approved-shadows must be a collection"
                          (focus-ring/make-group {:approved-shadows approved}))
        (pr-str approved)))
  (doseq [values ['app.theme.button/shadow-brand-focused
                  "#516bff"
                  #{"app.theme.button/shadow-brand-focused"}
                  #{'shadow-brand-focused}
                  {:a 1}]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":focus-ring :approved-shadows must map each shadow"
                          (focus-ring/make-group {:approved-shadows {'app.styles.focus-rings/primary-shadow values}}))
        (pr-str values)))
  (doseq [approved [nil [] ['app.styles.focus-rings/primary-shadow]
                    {'app.styles.focus-rings/primary-shadow #{"rgb(0 0 0 / 0.5)" "#516bff"}}]]
    (is (some? (focus-ring/make-group {:approved-shadows approved}))
        (pr-str approved))))

(deftest shared-replaced-value-names-first-shadow
  (let [shared {:approved-shadows {'app.styles.focus-rings/zeta-shadow #{'app.theme.button/shadow-brand-focused "#516bff"}
                                   'app.styles.focus-rings/alpha-shadow #{'app.theme.button/shadow-brand-focused "#516bff"}}}]
    (doseq [value ["button-theme/shadow-brand-focused" "\"0 0 0 2px #516bff\""]]
      (is (= "Use app.styles.focus-rings/alpha-shadow." (hint-of shared (visible-ring ":box-shadow " value " " outline)))
          value))))

(deftest preset-in-cljc-checks-only-outline
  (let [source (str "(ns app.styles.focus-rings (:require [spade.core :refer [defclass]]))\n"
                    (visible-ring ":box-shadow \"0 0 0 2px #516bff\" " outline))]
    (is (= {} (flagged (:results (temp-project/run-group (focus-ring/make-group config)
                                                         {"app/styles/focus_rings.cljc" source}))))
        "hand-built ring in cljc preset")))
