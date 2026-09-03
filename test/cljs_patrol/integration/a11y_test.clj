(ns cljs-patrol.integration.a11y-test
  (:require
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.a11y :as a11y]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private fixture-dir "test/projects/a11y-app/src/blogapp")

(defn- rows [items]
  (frequencies (map :row items)))

(deftest img-alt-missing-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [img-alt-missing]} (first group-results)
        by-row (rows img-alt-missing)]

    (testing "flags [:img] with no attrs"
      (is (contains? by-row 28)
          "bad-no-attrs case — [:img] with no attrs slot"))

    (testing "flags [:img {...}] literal map without :alt"
      (is (contains? by-row 31)
          "bad-attrs-without-alt case — literal attrs map missing :alt key"))

    (testing "flags Hiccup .class shorthand without :alt"
      (is (contains? by-row 34)
          "bad-hiccup-class-without-alt case — :img.thumb with no :alt"))

    (testing "flags empty literal attrs map"
      (is (contains? by-row 38)
          "bad-empty-attrs case — [:img {}] literal empty map"))

    (testing "flags every :img on a line that has multiple siblings"
      (is (= 2 (get by-row 43))
          "bad-two-on-same-line case — two [:img] on row 43 both flagged"))

    (testing "flags [:img {::alt \"...\"}] — ::alt is not the HTML :alt attribute"
      (is (contains? by-row 47)
          "ok-namespaced-alt-key-does-not-satisfy case — ::foo/alt ≠ :alt"))

    (testing "does not flag [:img {:alt \"...\"}]"
      (is (not (contains? by-row 4))
          "ok-with-alt case — [:img {:src ... :alt \"Blogapp logo\"}]")
      (is (not (contains? by-row 12))
          "ok-hiccup-class-with-alt case — :img.hero with :alt")
      (is (not (contains? by-row 16))
          "ok-hiccup-id-with-alt case — :img#avatar with :alt"))

    (testing "does not flag decorative :alt \"\""
      (is (not (contains? by-row 8))
          "ok-decorative case — :alt \"\" is valid per WCAG for decorative images"))

    (testing "does not flag quoted-vector Hiccup"
      (is (not (contains? by-row 21))
          "ok-quoted-vector-skipped case — '[:img] is data, not a live Hiccup form"))

    (testing "does not flag meta-wrapped attrs (conservative)"
      (is (not (contains? by-row 25))
          "ok-meta-wrapped-attrs case — [:img ^:foo {...}] treated as :dynamic"))

    (testing "does not flag when attrs are a non-literal form (conservative)"
      (is (not (contains? by-row 51))
          "dynamic-attrs-skipped case — [:img (merge ...)] non-literal attrs"))

    (testing "every finding carries the :img tag as :kw"
      (is (every? #(= :img (:kw %)) img-alt-missing)))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) img-alt-missing)))

    (testing "every finding carries a :col so same-line siblings differ"
      (is (every? #(pos? (:col %)) img-alt-missing))
      (let [same-row-findings (filter #(= 43 (:row %)) img-alt-missing)
            same-row-cols (set (map :col same-row-findings))]
        (is (= 2 (count same-row-cols))
            "the two [:img] on row 43 have distinct :col values")))

    (testing "every finding carries a :form snippet showing the actual Hiccup vector"
      (is (every? #(re-find #"^\[:img" (:form %)) img-alt-missing)
          "every finding's :form starts with [:img"))))

(deftest invalid-tabindex-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [invalid-tabindex]} (first group-results)
        by-row (rows invalid-tabindex)]

    (testing "flags positive integer tabindex"
      (is (contains? by-row 17)
          "bad-positive-tabindex case — {:tabIndex 1}")
      (is (contains? by-row 20)
          "bad-large-positive-tabindex case — {:tabIndex 100} on :button"))

    (testing "flags kebab-case :tab-index with positive value"
      (is (contains? by-row 33)
          "bad-kebab-positive-tabindex case — {:tab-index 5}"))

    (testing "flags string tabindex value"
      (is (contains? by-row 23)
          "bad-string-tabindex case — {:tabIndex \"1\"} is not an int"))

    (testing "flags float tabindex value"
      (is (contains? by-row 26)
          "bad-float-tabindex case — {:tabIndex 1.5} is not an int"))

    (testing "flags keyword tabindex value"
      (is (contains? by-row 29)
          "bad-keyword-tabindex case — {:tabIndex :something} is not an int"))

    (testing "does not flag :tabIndex 0 (in tab order — correct)"
      (is (not (contains? by-row 4))
          "ok-tabindex-zero case — 0 is a valid tabindex"))

    (testing "does not flag :tabIndex -1 (programmatic focus — correct)"
      (is (not (contains? by-row 7))
          "ok-tabindex-negative case — -1 is a valid tabindex"))

    (testing "does not flag kebab-case with 0"
      (is (not (contains? by-row 10))
          "ok-tab-index-kebab-zero case — kebab-case with 0 is valid"))

    (testing "does not flag non-literal tabindex value (conservative)"
      (is (not (contains? by-row 14))
          "ok-dynamic-tabindex case — {:tabIndex n} value is a symbol"))

    (testing "finding carries the element tag (not always :img)"
      (let [tags (set (map :kw invalid-tabindex))]
        (is (contains? tags :div))
        (is (contains? tags :button))))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) invalid-tabindex)))

    (testing "every finding carries a :form snippet showing the vector"
      (is (every? #(re-find #"^\[:(?:div|button)" (:form %)) invalid-tabindex)
          "every finding's :form starts with the tag"))))

(deftest on-click-on-non-interactive-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [on-click-on-non-interactive]} (first group-results)
        by-row (rows on-click-on-non-interactive)]

    (testing "flags [:div {:on-click ...}] without role or keyboard handler"
      (is (contains? by-row 34)
          "bad-clickable-div case"))

    (testing "flags [:span {:on-click ...}]"
      (is (contains? by-row 37)
          "bad-clickable-span case"))

    (testing "flags [:li {:on-click ...}]"
      (is (contains? by-row 40)
          "bad-clickable-li case"))

    (testing "flags camelCase :onClick on non-interactive tag"
      (is (contains? by-row 46)
          "bad-camelcase-onclick-on-section case — :onClick on :section"))

    (testing "flags Hiccup class shorthand [:.card ...] (treated as :div)"
      (is (contains? by-row 57)
          "bad-clickable-class-shorthand case — :.card is :div.card"))

    (testing "flags Hiccup id shorthand [:#header ...] (treated as :div)"
      (is (contains? by-row 61)
          "bad-clickable-id-shorthand case — :#header is :div#header"))

    (testing "flags :role nil (not a real role at runtime)"
      (is (contains? by-row 65)
          "bad-role-nil-does-not-count case"))

    (testing "flags :role \"\" (empty string not a real role)"
      (is (contains? by-row 70)
          "bad-role-empty-string case"))

    (testing "flags :role \"presentation\" (explicitly removes semantics)"
      (is (contains? by-row 76)
          "bad-role-presentation case — role=\"presentation\" is worse than no role"))

    (testing "flags :role \"none\" (ARIA 1.1 synonym for presentation)"
      (is (contains? by-row 82)
          "bad-role-none case"))

    (testing "flags :on-key-down nil (no-op handler)"
      (is (contains? by-row 88)
          "bad-keyboard-handler-nil case"))

    (testing "flags :on-mouse-down on a non-interactive tag"
      (is (contains? by-row 94)
          "bad-mouse-down-on-div case"))

    (testing "flags :on-touch-start on a non-interactive tag"
      (is (contains? by-row 97)
          "bad-touch-start-on-div case"))

    (testing "flags :on-pointer-down on a non-interactive tag"
      (is (contains? by-row 100)
          "bad-pointer-down-on-span case"))

    (testing "does not flag :on-mouse-down + :on-key-down together"
      (is (not (contains? by-row 103))
          "ok-mouse-down-with-keydown case — keyboard handler is the escape hatch"))

    (testing "does not flag natively interactive :button with :on-click"
      (is (not (contains? by-row 5))
          "ok-button-click case — :button is inherently keyboard-accessible"))

    (testing "does not flag natively interactive :a with :on-click"
      (is (not (contains? by-row 10))
          "ok-anchor-click case — :a is inherently keyboard-accessible"))

    (testing "does not flag :div with :role + keyboard handler"
      (is (not (contains? by-row 14))
          "ok-div-with-role-and-keydown case"))

    (testing "does not flag :div with camelCase :onKeyDown"
      (is (not (contains? by-row 21))
          "ok-div-with-camelcase-onkeydown case — :onKeyDown counts as keyboard handler"))

    (testing "does not flag :div without any :on-click"
      (is (not (contains? by-row 27))
          "ok-div-without-onclick case"))

    (testing "does not flag :div with :role alone (lenient escape hatch)"
      (is (not (contains? by-row 51))
          "ok-clickable-div-with-role-only case"))

    (testing "does not flag when attrs are non-literal"
      (is (not (contains? by-row 31))
          "ok-dynamic-attrs case — [:div (merge ...)]"))

    (testing "every finding carries the offending tag as :kw"
      (is (every? #(contains? #{:div :span :li :section} (:kw %)) on-click-on-non-interactive)
          "fixture only exercises this 4-tag subset of non-interactive-tags"))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) on-click-on-non-interactive)))

    (testing "every finding carries a :form snippet"
      (is (every? #(re-find #"^\[:" (:form %)) on-click-on-non-interactive)))))

(deftest empty-interactive-element-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [empty-interactive-element]} (first group-results)
        by-file (group-by :file empty-interactive-element)
        rows-in-file (fn [path]
                       (set (map :row (get by-file (str fixture-dir "/" path)))))
        rows (rows-in-file "interactive_content.cljs")]

    (testing "flags [:button] with no children"
      (is (contains? rows 32)
          "bad-empty-button — no attrs, no children"))

    (testing "flags [:button {…}] with attrs but no children"
      (is (contains? rows 35)
          "bad-empty-button-attrs — attrs but no children"))

    (testing "flags [:a {:href …}] with no children"
      (is (contains? rows 38)
          "bad-empty-anchor — href but no accessible name"))

    (testing "flags bare [:a]"
      (is (contains? rows 41)
          "bad-empty-anchor-no-attrs"))

    (testing "flags [:button {:aria-label \"\"}] — empty string is no name"
      (is (contains? rows 45)
          "bad-button-empty-aria-label"))

    (testing "flags [:a {:aria-label nil}] — nil is no name"
      (is (contains? rows 50)
          "bad-anchor-nil-aria-label"))

    (testing "flags [:div {:role \"button\"}] with no body"
      (is (contains? rows 71)
          "bad-empty-div-role-button"))

    (testing "flags [:span {:role \"link\"}] with no body"
      (is (contains? rows 76)
          "bad-empty-span-role-link"))

    (testing "flags [:div {:role \"button\" :aria-label \"\"}] — empty name still fails"
      (is (contains? rows 82)
          "bad-empty-div-role-button-empty-aria-label"))

    (testing "flags [:div {:role :button}] — keyword form of role also counts"
      (is (contains? rows 89)
          "bad-empty-div-role-kw-button"))

    (testing "flags icon-only [:button [:svg]] — no visible text or aria-label"
      (is (contains? rows 99)
          "bad-icon-only-button"))

    (testing "flags icon-only [:a [:svg]] — no visible text or aria-label"
      (is (contains? rows 103)
          "bad-icon-only-anchor"))

    (testing "flags nested icon-only [:button [:span [:svg]]]"
      (is (contains? rows 108)
          "bad-nested-icon-only-button"))

    (testing "does not flag when the vector has text content"
      (is (not (contains? rows 4))
          "ok-button-with-text"))

    (testing "does not flag when a child vector is present"
      (is (not (contains? rows 7))
          "ok-button-with-child-vector"))

    (testing "does not flag icon-only :button with :aria-label"
      (is (not (contains? rows 12))
          "ok-icon-button-with-aria-label"))

    (testing "does not flag icon-only :a with :title"
      (is (not (contains? rows 17))
          "ok-icon-anchor-with-title"))

    (testing "does not flag :a with :aria-labelledby"
      (is (not (contains? rows 22))
          "ok-anchor-with-aria-labelledby"))

    (testing "does not flag :button whose body is a dynamic expression (conservative)"
      (is (not (contains? rows 28))
          "ok-button-with-dynamic-child"))

    (testing "does not flag [:div {:role \"button\"}] with body text"
      (is (not (contains? rows 54))
          "ok-div-role-button-with-text"))

    (testing "does not flag [:span {:role \"link\" :aria-label \"…\"}]"
      (is (not (contains? rows 58))
          "ok-span-role-link-with-aria-label"))

    (testing "does not flag [:div {:role \"presentation\"}] — non-interactive role"
      (is (not (contains? rows 64))
          "ok-div-role-presentation-empty"))

    (testing "does not flag [:div {:role dynamic}] (conservative)"
      (is (not (contains? rows 68))
          "ok-div-role-dynamic-empty"))

    (testing "does not flag [:div {:role :link}] with body text"
      (is (not (contains? rows 94))
          "ok-div-role-kw-link-with-text"))

    (testing "does not flag [:button [:svg {:aria-label \"…\"}]] — labelled icon"
      (is (not (contains? rows 113))
          "ok-button-icon-with-own-aria-label"))

    (testing "does not flag [:button [:svg] \"text\"] — icon + visible text"
      (is (not (contains? rows 117))
          "ok-button-with-text-and-icon"))

    (testing "every finding has :bugs tier"
      (is (every? #(= :bugs (:tier %)) empty-interactive-element)))

    (testing "every finding carries the offending tag as :kw"
      (is (every? #(contains? #{:a :button :div :span} (:kw %)) empty-interactive-element)))))

(deftest missing-accessible-name-fixture-test
  (testing "without :component-aliases config: only native :textarea is checked"
    (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
          {:keys [missing-accessible-name]} (first group-results)
          by-row (rows missing-accessible-name)]

      (testing "flags [:textarea {:placeholder ...}] with no aria-label / aria-labelledby"
        (is (contains? by-row 7)
            "bad-native-placeholder-only"))

      (testing "flags [:textarea] with no attrs"
        (is (contains? by-row 12)
            "bad-native-no-attrs"))

      (testing "does not flag [:textarea {:aria-label ...}]"
        (is (not (contains? by-row 16))
            "ok-native-aria-label"))

      (testing "does not flag [:textarea {:aria-labelledby ...}]"
        (is (not (contains? by-row 23))
            "ok-native-aria-labelledby"))

      (testing "does not flag dynamic :aria-label — optimistically accepted"
        (is (not (contains? by-row 28))
            "ok-native-dynamic-aria-label"))

      (testing "does not flag [ui/textarea ...] wrapper — no config"
        (is (not (contains? by-row 34))
            "bad-wrapper-alias-placeholder-only"))

      (testing "does not flag [textarea ...] refer'd wrapper — no config"
        (is (not (contains? by-row 44))
            "bad-wrapper-refer-placeholder-only"))

      (testing "flags [:div {:role \"dialog\"}] without an accessible name"
        (is (contains? by-row 51)
            "bad-native-dialog-role"))

      (testing "flags [:div {:role :dialog}] — Reagent stringifies keyword values"
        (is (contains? by-row 56)
            "bad-native-dialog-role-keyword"))

      (testing "flags [:section {:aria-modal true}] as a dialog"
        (is (contains? by-row 61)
            "bad-native-aria-modal"))

      (testing "flags native [:dialog {...}] element"
        (is (contains? by-row 66)
            "bad-native-dialog-tag"))

      (testing "does not flag dialogs that supply an accessible name"
        (is (not (contains? by-row 70))
            "ok-native-dialog-with-aria-label")
        (is (not (contains? by-row 75))
            "ok-native-dialog-with-aria-labelledby"))

      (testing "does not flag [:div {:role role-var}] — dynamic role skipped"
        (is (not (contains? by-row 82))
            "ok-native-dynamic-role"))

      (testing "does not flag [ui/dialog-root ...] wrapper — no config"
        (is (not (contains? by-row 87))
            "bad-wrapper-dialog"))

      (testing "every finding has :bugs tier"
        (is (every? #(= :bugs (:tier %)) missing-accessible-name)))))

  (testing "with :component-aliases config: wrapper calls also participate"
    (let [configured (a11y/make-group
                      {:component-aliases {'blogapp.ui/textarea :textarea
                                           'blogapp.ui/dialog-root :dialog}})
          {:keys [group-results]} (core/run fixture-dir [configured])
          {:keys [missing-accessible-name]} (first group-results)
          by-row (rows missing-accessible-name)]

      (testing "flags [ui/textarea {:placeholder ...}] once the wrapper is mapped"
        (is (contains? by-row 34)
            "bad-wrapper-alias-placeholder-only"))

      (testing "flags [textarea {:placeholder ...}] via :refer resolution"
        (is (contains? by-row 44)
            "bad-wrapper-refer-placeholder-only"))

      (testing "flags [ui/dialog-root {...}] wrapper mapped to :dialog"
        (is (contains? by-row 87)
            "bad-wrapper-dialog"))

      (testing "still does not flag [ui/textarea {:aria-label ...}]"
        (is (not (contains? by-row 39))
            "ok-wrapper-alias-aria-label"))

      (testing "still does not flag [ui/dialog-root {:aria-label ...}]"
        (is (not (contains? by-row 91))
            "ok-wrapper-dialog-with-aria-label"))

      (testing "still flags native cases"
        (is (contains? by-row 7))
        (is (contains? by-row 12))
        (is (contains? by-row 51))
        (is (contains? by-row 66))))))

(deftest button-wrapper-fixture-test
  (let [buttons-file (str fixture-dir "/buttons.cljs")]
    (testing "without :component-aliases config: wrapper buttons are inert"
      (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
            {:keys [empty-interactive-element]} (first group-results)]
        (is (empty? (filter #(= buttons-file (:file %)) empty-interactive-element))
            "no wrapper is mapped — buttons.cljs contributes no findings")))

    (testing "with :component-aliases {blogapp.ui/button :button}"
      (let [configured (a11y/make-group
                        {:component-aliases {'blogapp.ui/button :button}})
            {:keys [group-results]} (core/run fixture-dir [configured])
            {:keys [empty-interactive-element]} (first group-results)
            in-buttons (filter #(= buttons-file (:file %)) empty-interactive-element)
            rows (set (map :row in-buttons))]

        (testing "flags [ui/button {:icon …}] — icon-only wrapper, no accessible name"
          (is (contains? rows 6)
              "bad-icon-only-wrapper"))

        (testing "flags [button {:icon …}] via :refer resolution"
          (is (contains? rows 11)
              "bad-icon-only-wrapper-refer"))

        (testing "does not flag icon wrapper carrying :aria-label"
          (is (not (contains? rows 15))
              "ok-icon-with-aria-label"))

        (testing "does not flag icon wrapper with visible text child"
          (is (not (contains? rows 20))
              "ok-icon-with-visible-child"))

        (testing "does not flag wrapper with only text (no :icon)"
          (is (not (contains? rows 25))
              "ok-text-only"))

        (testing "does not flag wrapper with both :icon and visible text child"
          (is (not (contains? rows 29))
              "ok-icon-and-text"))

        (testing "every wrapper finding carries :kw :button and :bugs tier"
          (is (every? #(= :button (:kw %)) in-buttons))
          (is (every? #(= :bugs (:tier %)) in-buttons)))))))

(deftest live-region-missing-aria-live-fixture-test
  (let [{:keys [group-results]} (core/run fixture-dir [a11y/group])
        {:keys [live-region-missing-aria-live]} (first group-results)
        by-row (rows live-region-missing-aria-live)]

    (testing "flags a live-region role with no :aria-live at all"
      (is (contains? by-row 37)
          "bad-status-without-aria-live case — :role \"status\" alone")
      (is (contains? by-row 40)
          "bad-alert-without-aria-live case — :role \"alert\" alone, on a :span")
      (is (contains? by-row 43)
          "bad-log-without-aria-live case — :role \"log\" alone"))

    (testing "flags the keyword spelling of the role"
      (is (contains? by-row 46)
          "bad-keyword-role-without-aria-live case — :role :status is stringified by Reagent"))

    (testing "flags an :aria-live that contradicts the role's implicit value"
      (is (contains? by-row 49)
          "bad-alert-downgraded-to-polite case — \"alert\" implies \"assertive\"")
      (is (contains? by-row 53)
          "bad-status-upgraded-to-assertive case — \"status\" implies \"polite\"")
      (is (contains? by-row 57)
          "bad-keyword-value-mismatch case — :alert with :polite"))

    (testing "flags an explicit nil :aria-live, which announces nothing"
      (is (contains? by-row 61)
          "bad-explicit-nil-aria-live case — literal nil reads as absent"))

    (testing "does not flag a role whose :aria-live matches"
      (is (not (contains? by-row 4))
          "ok-status-with-polite case")
      (is (not (contains? by-row 8))
          "ok-alert-with-assertive case")
      (is (not (contains? by-row 12))
          "ok-log-with-polite case")
      (is (not (contains? by-row 16))
          "ok-keyword-spellings case — :status with :polite"))

    (testing "does not flag a role that is not a live region"
      (is (not (contains? by-row 20))
          "ok-not-a-live-region case — :role \"navigation\""))

    (testing "does not flag non-literal values (conservative)"
      (is (not (contains? by-row 24))
          "ok-dynamic-role case — role is a symbol")
      (is (not (contains? by-row 28))
          "ok-dynamic-aria-live case — :aria-live is a symbol")
      (is (not (contains? by-row 33))
          "ok-computed-aria-live case — :aria-live is an (if ...) form"))

    (testing "flags exactly the bad- cases in the fixture"
      (is (= 8 (count (filter #(str/ends-with? (:file %) "live_regions.cljs")
                              live-region-missing-aria-live)))
          "eight bad- cases, no more"))))
