(ns cljs-patrol.groups.focus-ring-test
  (:require
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.focus-ring :as focus-ring]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.test :refer [deftest is]]))

(def ^:private config
  {:approved-shadows #{'app.styles.focus-rings/primary-shadow}
   :allow-stacking-under #{":focus-visible"}
   :required-companion {:outline "2px solid transparent"}
   :forbidden-tokens {'app.theme.button/shadow-brand-focused 'app.styles.focus-rings/primary-shadow}
   :forbidden-color-literals #{"#516bff"}})

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

(deftest approved-ring-with-companion-passes
  (is (= {} (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring " " outline)))))
      "approved shadow with outline"))

(deftest ring-without-companion-is-flagged
  (let [results (run (focus-class "a" "\"&:focus-visible::after\"" (str ":box-shadow " ring)))]
    (is (= {:focus-ring-missing-companion ["a"]} (flagged results))
        "missing outline")
    (is (= "Set :outline \"2px solid transparent\" beside :box-shadow."
           (-> results :focus-ring-missing-companion first :hint))
        "hint names exact companion value")))

(deftest companion-with-wrong-value-is-flagged
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring " :outline :none")))))
      "outline none")
  (is (= {}
         (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring " :outline \"2px  solid transparent\"")))))
      "extra whitespace in value"))

(deftest companion-read-from-symbol-is-accepted
  (is (= {} (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring " :outline focus-rings/outline")))))
      "symbol companion value"))

(deftest partial-companion-is-flagged
  (let [two-properties (assoc config :required-companion {:outline "2px solid transparent"
                                                          :outline-offset "2px"})
        results (run two-properties (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring " " outline)))]
    (is (= {:focus-ring-missing-companion ["a"]} (flagged results))
        "one of two properties set")
    (is (= "Set :outline-offset \"2px\" beside :box-shadow."
           (-> results :focus-ring-missing-companion first :hint))
        "hint names only missing property")))

(deftest companion-in-another-map-of-block-passes
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "} {:box-shadow " ring "}])"))))
      "two maps in one block"))

(deftest companion-in-enclosing-focus-block-passes
  (is (= {} (flagged (run (str "(defclass a [] [:&:focus-visible {" outline "} [:&::after {:box-shadow " ring "}]])"))))
      "outline on enclosing focus block")
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (str "(defclass a [] [:&:hover {" outline "} [:&:focus-visible {:box-shadow " ring "}]])"))))
      "outline on enclosing hover block"))

(deftest block-built-by-call-skips-companion
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
    (is (= {:focus-ring-missing-companion ["a"]} (flagged (run (focus-class "a" selector (str ":box-shadow " ring)))))
        selector))
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (str "(defclass a [] [:&:focus-visible [:.icon {:box-shadow " ring "}]])"))))
      "nested under focus block")
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (str "(defclass a [] [:&:hover :&:focus-visible {:box-shadow " ring "}])"))))
      "second selector of block"))

(deftest configured-selectors-replace-defaults
  (let [only-visible (assoc config :selectors #{":focus-visible"})]
    (is (= {} (flagged (run only-visible (focus-class "a" ":&:focus" (str ":box-shadow " ring)))))
        "focus not configured")
    (is (= {:focus-ring-missing-companion ["a"]}
           (flagged (run only-visible (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring)))))
        "focus-visible configured"))
  (is (= {} (flagged (run (assoc config :selectors #{":focus"}) (focus-class "a" ":&:focus-visible" (str ":box-shadow " ring)))))
      "focus not matched inside focus-visible"))

(deftest map-entries-are-read-past-discards-and-string-keys
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str "\"box-shadow\" " ring)))))
      "string key")
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str "#_:color :box-shadow " ring)))))
      "discarded key before entries"))

(deftest declaration-kinds-are-read
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (str "(defattrs a [] [:&:focus-visible {:box-shadow " ring "}])"))))
      "defattrs")
  (is (= {:focus-ring-missing-companion ["a"]}
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
                 "(str \"0 0 0 2px \" color)"]]
    (is (= {:focus-ring-raw-shadow ["a"]} (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow " value " " outline)))))
        value)))

(deftest stacked-ring-depends-on-state
  (let [stacked (str ":box-shadow (str highlight \", \" " ring ") " outline)]
    (is (= {} (flagged (run (focus-class "a" ":&:focus-visible" stacked))))
        "stacking allowed under focus-visible")
    (is (= {:focus-ring-raw-shadow ["a"]} (flagged (run (focus-class "a" ":&:focus" stacked))))
        "stacking not allowed under focus")
    (is (= {:focus-ring-raw-shadow ["a"]}
           (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow (str highlight \", \" other) " outline)))))
        "str without approved shadow")))

(deftest shadow-resolves-through-refer-and-full-name
  (let [referred (str ":box-shadow danger-shadow " outline)]
    (is (= {:focus-ring-raw-shadow ["a"]} (flagged (run (focus-class "a" ":&:focus-visible" referred))))
        "referred name not approved")
    (is (= {} (flagged (run (update config :approved-shadows conj 'app.styles.focus-rings/danger-shadow)
                            (focus-class "a" ":&:focus-visible" referred))))
        "referred name approved"))
  (is (= {:focus-ring-forbidden-token ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow app.theme.button/shadow-brand-focused " outline)))))
      "fully qualified forbidden token")
  (is (= {} (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow app.styles.focus-rings/primary-shadow " outline)))))
      "fully qualified approved shadow"))

(deftest unreadable-shadow-skips-source-check
  (is (= {} (flagged (run (str "(defclass a [ring] [:&:focus-visible {:box-shadow ring " outline "}])"))))
      "local")
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run "(defclass a [ring] [:&:focus-visible {:box-shadow ring}])")))
      "local still needs companion")
  (is (= {} (flagged (run (focus-class "a" ":&:focus-visible" "\n:box-shadow (if highlighted? x y)"))))
      "call other than str"))

(deftest custom-property-reference-is-read-like-local
  (is (= {} (flagged (run (focus-class "a" ":&:focus" (str ":box-shadow \"var(--ring) !important\" " outline)))))
      "custom property alone")
  (is (= {:focus-ring-missing-companion ["a"]}
         (flagged (run (focus-class "a" ":&:focus" ":box-shadow \"var(--ring)\""))))
      "custom property still needs companion")
  (is (= {:focus-ring-forbidden-color ["a"]}
         (flagged (run (focus-class "a" ":&:focus" (str ":box-shadow \"var(--ring, 0 0 0 2px #516bff)\" " outline)))))
      "custom property with literal fallback"))

(deftest forbidden-token-is-flagged
  (let [results (run (focus-class "a" ":&:focus-visible" (str ":box-shadow button-theme/shadow-brand-focused " outline)))]
    (is (= {:focus-ring-forbidden-token ["a"]} (flagged results))
        "token replaces raw-shadow finding")
    (is (= "Use app.styles.focus-rings/primary-shadow." (-> results :focus-ring-forbidden-token first :hint))
        "hint names mapped shadow"))
  (is (= {:focus-ring-forbidden-token ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow (str \"inset \" button-theme/shadow-brand-focused) " outline)))))
      "token inside str")
  (let [results (run (assoc config :forbidden-tokens #{'app.theme.button/shadow-brand-focused})
                     (focus-class "a" ":&:focus-visible" (str ":box-shadow button-theme/shadow-brand-focused " outline)))]
    (is (= {:focus-ring-forbidden-token ["a"]} (flagged results))
        "token from set")
    (is (nil? (-> results :focus-ring-forbidden-token first :hint))
        "no hint without mapping")))

(deftest forbidden-color-is-flagged
  (is (= {:focus-ring-forbidden-color ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow \"0 0 0 2px #516BFF\" " outline)))))
      "upper-case color")
  (is (= {:focus-ring-forbidden-color ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" (str ":box-shadow (str \"0 0 0 2px \" \"#516bff\") " outline)))))
      "color inside str")
  (is (= {:focus-ring-forbidden-color ["a"]}
         (flagged (run (assoc config :forbidden-color-literals #{"#516BFF"})
                       (focus-class "a" ":&:focus-visible" (str ":box-shadow \"0 0 0 2px #516bff\" " outline)))))
      "upper-case color in config")
  (is (= {:focus-ring-forbidden-color ["a"]
          :focus-ring-missing-companion ["a"]}
         (flagged (run (focus-class "a" ":&:focus-visible" ":box-shadow \"0 0 0 2px #516bff\""))))
      "color and missing companion"))

(deftest exempt-path-checks-only-companion
  (let [exempt (assoc config :exempt-paths #{"app/core.cljs"})]
    (is (= {} (flagged (run exempt (focus-class "a" ":&:focus-visible" (str ":box-shadow \"0 0 0 2px #516bff\" " outline)))))
        "hand-built ring in exempt file")
    (is (= {:focus-ring-missing-companion ["a"]}
           (flagged (run exempt (focus-class "a" ":&:focus-visible" ":box-shadow \"0 0 0 2px #516bff\""))))
        "companion still required")
    (is (= {:focus-ring-forbidden-color ["a"]}
           (flagged (run exempt "app/other.cljs" (focus-class "a" ":&:focus-visible" (str ":box-shadow \"0 0 0 2px #516bff\" " outline)))))
        "other file")))

(deftest empty-keys-turn-off-their-checks
  (let [source (focus-class "a" ":&:focus-visible" ":box-shadow \"0 0 0 2px #516bff\"")]
    (is (= {:focus-ring-raw-shadow ["a"]}
           (flagged (run {:approved-shadows #{'app.styles.focus-rings/primary-shadow}} source)))
        "only approved shadows")
    (is (= {:focus-ring-missing-companion ["a"]}
           (flagged (run {:required-companion {:outline "2px solid transparent"}} source)))
        "only companion")
    (is (= {} (flagged (run {} source)))
        "empty config")))

(deftest unconfigured-group-does-nothing
  (is (= {} (group/parse-handlers focus-ring/group))
      "no handlers")
  (is (empty? (group/summary-lines focus-ring/group {}))
      "no summary lines")
  (is (= 4 (count (group/summary-lines (focus-ring/make-group {}) {})))
      "summary lines once configured")
  (is (= {} (flagged (:results (temp-project/run-group focus-ring/group
                                                       {"app/core.cljs" (str ns-form (focus-class "a" ":&:focus-visible" ":box-shadow \"0 0 0 2px #516bff\""))}))))
      "no findings"))

(deftest any-finding-fails
  (doseq [rule (keys (group/rule->tier focus-ring/group))]
    (is (group/failed? focus-ring/group {rule [{:kw :app/x}]})
        (name rule)))
  (is (not (group/failed? focus-ring/group {:focus-ring-missing-companion []}))
      "empty results"))
