(ns cljs-patrol.groups.a11y-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.disk :as disk]
   [cljs-patrol.group :as group]
   [cljs-patrol.groups.a11y :as a11y]
   [cljs-patrol.groups.a11y.roles :as roles]
   [cljs-patrol.parser :as parser]
   [cljs-patrol.temp-dir :as temp-dir]
   [cljs-patrol.temp-project :as temp-project]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn- img-finding [row]
  {:type :img-alt-missing
   :kw :img
   :file "views.cljs"
   :row row})

(defn- tabindex-finding [row]
  {:type :invalid-tabindex
   :kw :div
   :file "views.cljs"
   :row row})

(defn- button-type-finding [row]
  {:type :button-missing-type
   :kw :button
   :file "views.cljs"
   :row row})

(defn- on-click-finding [row]
  {:type :on-click-on-non-interactive
   :kw :div
   :file "views.cljs"
   :row row})

(defn- empty-interactive-finding [row]
  {:type :empty-interactive-element
   :kw :button
   :file "views.cljs"
   :row row})

(defn- accessible-name-finding [row]
  {:type :missing-accessible-name
   :kw :textarea
   :file "views.cljs"
   :row row})

(defn- repeated-name-finding [row]
  {:type :repeated-accessible-name
   :kw :button
   :file "views.cljs"
   :row row})

(defn- contradicting-aria-live-finding [row]
  {:type :aria-live-contradicts-role
   :kw :div
   :file "views.cljs"
   :row row})

(deftest analyze-test
  (testing "no usages — no findings"
    (let [result (group/analyze a11y/group {:usages []})]
      (is (empty? (:img-alt-missing result)))
      (is (empty? (:invalid-tabindex result)))
      (is (empty? (:on-click-on-non-interactive result)))
      (is (empty? (:empty-interactive-element result)))
      (is (empty? (:repeated-accessible-name result)))
      (is (empty? (:aria-live-contradicts-role result)))))

  (testing "splits usages by :type across all rules"
    (let [other {:type :some-other-thing
                 :kw :div
                 :file "v.cljs"
                 :row 1}
          result (group/analyze a11y/group
                                {:usages [(img-finding 3)
                                          (tabindex-finding 5)
                                          (on-click-finding 7)
                                          (empty-interactive-finding 9)
                                          (repeated-name-finding 10)
                                          (contradicting-aria-live-finding 11)
                                          other
                                          (img-finding 8)]})]
      (is (= 2 (count (:img-alt-missing result))))
      (is (= #{3 8} (->> result :img-alt-missing (map :row) set)))
      (is (= 1 (count (:invalid-tabindex result))))
      (is (= #{5} (->> result :invalid-tabindex (map :row) set)))
      (is (= 1 (count (:on-click-on-non-interactive result))))
      (is (= #{7} (->> result :on-click-on-non-interactive (map :row) set)))
      (is (= 1 (count (:empty-interactive-element result))))
      (is (= #{9} (->> result :empty-interactive-element (map :row) set)))
      (is (= 1 (count (:repeated-accessible-name result))))
      (is (= #{10} (->> result :repeated-accessible-name (map :row) set)))
      (is (= 1 (count (:aria-live-contradicts-role result))))
      (is (= #{11} (->> result :aria-live-contradicts-role (map :row) set))))))

(deftest failed?-test
  (testing "fails when any img is missing alt"
    (is (group/failed? a11y/group {:img-alt-missing [(img-finding 5)]
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []})))

  (testing "fails when any tabindex is invalid"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex [(tabindex-finding 5)]
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []})))

  (testing "fails when any button-missing-type is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :button-missing-type [(button-type-finding 5)]
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []})))

  (testing "fails when any on-click-on-non-interactive is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive [(on-click-finding 5)]
                                   :empty-interactive-element []})))

  (testing "fails when any empty-interactive-element is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element [(empty-interactive-finding 5)]})))

  (testing "fails when any missing-accessible-name is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []
                                   :missing-accessible-name [(accessible-name-finding 5)]})))

  (testing "fails when any repeated-accessible-name is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []
                                   :missing-accessible-name []
                                   :repeated-accessible-name [(repeated-name-finding 5)]})))

  (testing "fails when any aria-live-contradicts-role is found"
    (is (group/failed? a11y/group {:img-alt-missing []
                                   :invalid-tabindex []
                                   :on-click-on-non-interactive []
                                   :empty-interactive-element []
                                   :missing-accessible-name []
                                   :aria-live-contradicts-role [(contradicting-aria-live-finding 5)]})))

  (testing "fails when any invalid-role is found"
    (is (group/failed? a11y/group {:invalid-role [{:file "app/view.cljs"
                                                   :line 5}]})))

  (testing "does not fail when clean"
    (is (not (group/failed? a11y/group {:img-alt-missing []
                                        :invalid-tabindex []
                                        :on-click-on-non-interactive []
                                        :empty-interactive-element []
                                        :missing-accessible-name []
                                        :repeated-accessible-name []
                                        :aria-live-contradicts-role []})))))

(deftest summary-lines-test
  (let [lines (group/summary-lines a11y/group
                                   {:img-alt-missing [(img-finding 1) (img-finding 2)]
                                    :invalid-tabindex [(tabindex-finding 3)]
                                    :button-missing-type [(button-type-finding 9) (button-type-finding 10)
                                                          (button-type-finding 11)]
                                    :on-click-on-non-interactive [(on-click-finding 4) (on-click-finding 5)]
                                    :empty-interactive-element [(empty-interactive-finding 6)]
                                    :missing-accessible-name []
                                    :repeated-accessible-name [(repeated-name-finding 7)]
                                    :aria-live-contradicts-role [(contradicting-aria-live-finding 8)]
                                    :aria-hidden-focusable []
                                    :nested-interactive-element []
                                    :label-not-associated []
                                    :aria-label-beside-visible-label []})]
    (is (= 15 (count lines)))
    (is (= 2 (second (first lines))))
    (is (= 1 (second (second lines))))
    (is (= ["Button missing type:" 3] (nth lines 2)))
    (is (= 2 (second (nth lines 3))))
    (is (= 1 (second (nth lines 4))))
    (is (= 1 (second (nth lines 6))))
    (is (= 1 (second (nth lines 7))))))

(defn- role-findings
  ([source] (role-findings nil source))
  ([config source]
   (get-in (temp-project/run-group (a11y/make-group config)
                                   {"app/view.cljs" (str "(ns app.view (:require [example.ui :as ui]))\n" source)})
           [:results :invalid-role])))

(deftest invalid-role-rejects-unknown-abstract-and-empty-literals
  (doseq [value (concat ["buton" "button buton" "buton button" "button widget" "Button" "BUTTON"
                         "" " \t\n\f\r " "doc-madeup" "graphics-madeup" "button\u000bcheckbox"
                         "button\u00a0checkbox" :buton 42 true \b]
                        (str/split "command composite input landmark range roletype section sectionhead select structure widget window"
                                   #" "))]
    (testing (pr-str value)
      (let [form (str "[:div {:role " (pr-str value) "}]")
            findings (role-findings (str "(defn view [] " form ")"))]
        (is (= 1 (count findings)))
        (is (= (str ":role " (pr-str value) " is invalid; use non-abstract ARIA role tokens in lowercase.")
               (:hint (first findings))))))))

(deftest invalid-role-accepts-the-complete-role-vocabulary
  (let [expected (str/split
                  (str "alert alertdialog application article banner blockquote button caption cell checkbox code columnheader "
                       "combobox complementary contentinfo definition deletion dialog directory document emphasis feed figure form "
                       "generic grid gridcell group heading img insertion link list listbox listitem log main marquee math menu "
                       "menubar menuitem menuitemcheckbox menuitemradio meter navigation none note option paragraph presentation "
                       "progressbar radio radiogroup region row rowgroup rowheader scrollbar search searchbox separator slider "
                       "spinbutton status strong subscript superscript switch tab table tablist tabpanel term textbox time timer "
                       "toolbar tooltip tree treegrid treeitem "
                       "doc-abstract doc-acknowledgments doc-afterword doc-appendix doc-backlink doc-biblioentry doc-bibliography "
                       "doc-biblioref doc-chapter doc-colophon doc-conclusion doc-cover doc-credit doc-credits doc-dedication "
                       "doc-endnote doc-endnotes doc-epigraph doc-epilogue doc-errata doc-example doc-footnote doc-foreword "
                       "doc-glossary doc-glossref doc-index doc-introduction doc-noteref doc-notice doc-pagebreak doc-pagelist "
                       "doc-part doc-preface doc-prologue doc-pullquote doc-qna doc-subtitle doc-tip doc-toc "
                       "graphics-document graphics-object graphics-symbol")
                  #" ")]
    (is (= (set expected) roles/valid-roles))
    (is (= 124 (count roles/valid-roles)))
    (is (empty? (role-findings
                 (str "(defn view [] [:div "
                      (str/join " " (for [role expected
                                          value [role (keyword role)]]
                                      (str "[:div {:role " (pr-str value) "}]")))
                      "])")))))
  (is (empty? (role-findings "(defn view [] [:div {:role \" \tbutton\n\f\r checkbox  \"}])"))))

(deftest role-vocabulary-includes-the-groups-existing-role-sets
  (doseq [values [a11y/empty-interactive-roles a11y/focusable-roles a11y/name-required-roles
                  a11y/widget-role-tags a11y/field-roles a11y/checkbox-role-values
                  a11y/separator-roles (keys a11y/role->implicit-aria-live)
                  (disj a11y/no-op-role-values nil "")]
          role values]
    (is (contains? roles/valid-roles (name role)) (pr-str role))))

(deftest invalid-role-leaves-dynamic-values-components-and-data-alone
  (doseq [value ["role" "(choose-role)" "(if active? \"button\" \"group\")"
                 "#?(:cljs \"buton\")" "^{:dynamic true} \"buton\"" "[\"buton\"]"
                 "{:name \"buton\"}" "#{\"buton\"}" "nil" "false"]]
    (is (empty? (role-findings (str "(defn view [role active?] [:div {:role " value "}])"))) value))
  (doseq [source ["(defn view [] [ui/panel {:role \"buton\"}])"
                  "(defn view [] [:> ui/panel {:role \"buton\"}])"
                  "(defn view [] '[ :div {:role \"buton\"}])"
                  "(def schema [:map [:div {:role \"buton\"} :string]])"
                  "(defn style [] [:div {:role \"buton\"} [:&:hover {:color \"red\"}]])"
                  "(defn view [] (get-in props [:div {:role \"buton\"}]))"
                  "(s/defclass panel [] [:div [:span {:role \"buton\"}]])"
                  "(defn view [props] [:div (merge {:role \"buton\"} props)])"
                  "(defn view [props] [:div props])"
                  "(defn view [] [:div {}])"
                  "(defn view [] [:div])"]]
    (is (empty? (role-findings {:component-aliases {'example.ui/panel :div}}
                               source)) source)))

(deftest invalid-role-reports-native-tags-and-settled-attrs
  (let [findings (role-findings
                  (str "(defn view [props] [:main\n"
                       " [:span.panel#notice {:role :widget}]\n"
                       " [:div (assoc props :role \"buton\")]])"))
        result {:invalid-role findings}]
    (is (= [[:span 3 2] [:div 4 2]] (mapv (juxt :kw :row :col) findings)))
    (is (= [:bugs :bugs] (mapv :tier findings)))
    (is (= ["[:span.panel#notice {:role :widget}]" "[:div (assoc props :role \"buton\")]"]
           (mapv :form findings)))
    (is (= :invalid-role (:rule (baseline/issue->identity :invalid-role (first findings)))))
    (is (group/failed? a11y/group result))
    (is (some #{["Invalid role:" 2]} (group/summary-lines a11y/group result)))))

(defn- spelling-results [config source]
  (:results (temp-project/run-group (a11y/make-group config)
                                    {"app/view.cljs" (str "(ns app.view (:require [example.ui :as ui]))\n" source)})))

(deftest role-value-spelling-modes
  (doseq [[mode value expected] [[:string :group ["Write :role \"group\"."]]
                                 [:string :x/group ["Write :role \"group\"."]]
                                 [:keyword "group" ["Write :role :group."]]
                                 [:keyword " \tgroup\n\f\r " ["Write :role :group."]]
                                 [:keyword :x/group ["Write :role :group."]]
                                 [:mixed :x/group ["Write :role :group."]]
                                 [:string "group" []]
                                 [:string "switch checkbox" []]
                                 [:keyword :group []]
                                 [:keyword "switch checkbox" []]
                                 [:keyword "switch\tcheckbox" []]
                                 [:mixed :group []]
                                 [:mixed "group" []]
                                 [:mixed "switch checkbox" []]]]
    (testing (pr-str [mode value])
      (let [result (spelling-results {:role-value mode} (str "(defn view [] [:div {:role " (pr-str value) "}])"))
            findings (:role-value-spelling result)]
        (is (= expected (mapv :hint findings)))
        (is (empty? (:invalid-role result)))))))

(deftest role-value-spelling-is-off-until-configured
  (let [result (spelling-results {}
                                 (str "(defn view [] [:main [:div {:role :group}] "
                                      "[:div {:role \"group\"}] [:div {:role :x/group}]])"))]
    (is (empty? (:role-value-spelling result)))
    (is (empty? (:invalid-role result)))))

(deftest role-value-spelling-defers-to-invalid-role
  (doseq [mode [:string :keyword :mixed]
          value [:buton :x/buton :x/widget "buton" "group buton" "" " \t\n\f\r " "Group" 42 true \g]]
    (testing (pr-str [mode value])
      (let [result (spelling-results {:role-value mode} (str "(defn view [] [:div {:role " (pr-str value) "}])"))]
        (is (empty? (:role-value-spelling result)))
        (is (= 1 (count (:invalid-role result))))))))

(deftest role-value-spelling-leaves-dynamic-values-components-and-data-alone
  (doseq [mode [:string :keyword :mixed]
          source ["(defn view [role] [:div {:role role}])"
                  "(defn view [] [:div {:role (choose-role)}])"
                  "(defn view [] [:div {:role #?(:cljs :x/group)}])"
                  "(defn view [] [:div {:role ^:dynamic :x/group}])"
                  "(defn view [] [:div {:role [:x/group]}])"
                  "(defn view [] [:div {:role nil}])"
                  "(defn view [] [:div {:role false}])"
                  "(defn view [] [ui/panel {:role :x/group}])"
                  "(defn view [] [:> ui/panel {:role :x/group}])"
                  "(defn view [] '[:div {:role :x/group}])"
                  "(def schema [:map [:div {:role :x/group} :string]])"
                  "(defn style [] [:div {:role :x/group} [:&:hover {:color \"red\"}]])"
                  "(defn view [] (get-in props [:div {:role :x/group}]))"
                  "(s/defclass panel [] [:div [:span {:role :x/group}]])"
                  "(defn view [props] [:div (merge {:role :x/group} props)])"
                  "(defn view [props] [:div props])"
                  "(defn view [] [:div {}])"
                  "(defn view [] [:div])"]]
    (is (empty? (:role-value-spelling (spelling-results {:role-value mode
                                                         :component-aliases {'example.ui/panel :div}}
                                                        source)))
        (pr-str [mode source]))))

(deftest role-value-spelling-reports-native-tags-and-settled-attrs
  (let [result (spelling-results {:role-value :string}
                                 (str "(defn view [props] [:main\n"
                                      " [:span.panel#notice {:role :group}]\n"
                                      " [:div (assoc props :role :group)]\n"
                                      " [:div (merge props {:role :group})]\n"
                                      " (let [attrs {:role :group}] [:div attrs])])"))
        findings (:role-value-spelling result)
        first-finding (first findings)
        identity (baseline/issue->identity :role-value-spelling first-finding)]
    (is (= [[:span 3 2] [:div 4 2] [:div 5 2] [:div 6 30]] (mapv (juxt :kw :row :col) findings)))
    (is (= [:cleanup :cleanup :cleanup :cleanup] (mapv :tier findings)))
    (is (= "[:span.panel#notice {:role :group}]" (:form first-finding)))
    (is (= {:rule :role-value-spelling
            :tag :span
            :file (:file first-finding)
            :form (:form first-finding)}
           identity))
    (is (= identity (baseline/issue->identity :role-value-spelling (assoc first-finding :row 100 :col 50))))
    (is (not (group/failed? a11y/group result)))
    (is (= ["Role value spelling:" 4] (last (group/summary-lines a11y/group result))))))

(deftest role-value-option-rejects-malformed-values
  (doseq [value [nil false true :strings :app/string "string" 'string [:string] #{:keyword} {:value :mixed} 1]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":a11y :role-value must be :string, :keyword or :mixed"
                          (a11y/make-group {:role-value value}))
        (pr-str value))))

(defn- tooltip-findings [config source]
  (get-in (temp-project/run-group (a11y/make-group config)
                                  {"app/view.cljs"
                                   (str "(ns app.view (:require [example.ui :as ui :refer [tooltip]] "
                                        "[\"@radix-ui/react-tooltip\" :as Tooltip] "
                                        "[\"example-tooltip\" :as Other]))\n" source)})
          [:results :non-focusable-tooltip-trigger]))

(def ^:private tooltip-config {:tooltip-components #{'example.ui/tooltip}})

(deftest plain-tooltip-wrappers-are-reported-at-the-child
  (let [findings (tooltip-findings tooltip-config
                                   (str "[tooltip {:content \"Details\"}\n"
                                        " [:span {:role \"button\"} [:button {:type \"button\"} \"More\"]]]\n"
                                        "[ui/tooltip nil [:div.panel \"Info\"]]\n"
                                        "[:> ui/tooltip props [:span \"Details\"]]"))
        identities (map #(baseline/issue->identity :non-focusable-tooltip-trigger %) findings)]
    (is (= [[:span 3 2] [:div 4 17] [:span 5 22]]
           (mapv (juxt :kw :row :col) findings)))
    (is (= 3 (count (set identities))))
    (is (every? :inner-form identities))
    (is (group/failed? a11y/group {:non-focusable-tooltip-trigger findings}))))

(deftest tooltip-wrappers-require-an-unshadowed-configured-component
  (is (empty? (tooltip-findings nil "[tooltip {} [:span \"Details\"]]")))
  (is (empty? (tooltip-findings tooltip-config
                                (str "(let [tooltip render] [tooltip {} [:span \"Local\"]]) "
                                     "'[tooltip {} [:span \"Data\"]] "
                                     "[other/tooltip {} [:span \"Unknown\"]] "
                                     "[tooltip {} [:span \"One\"] [:span \"Two\"]] "
                                     "[tooltip {}]")))))

(deftest explicit-radix-as-child
  (let [findings (tooltip-findings nil
                                   (str "[:> Tooltip/Trigger {:as-child true} [:div \"Details\"]]\n"
                                        "[:> Tooltip/Trigger {:asChild true} [:span \"Details\"]]\n"
                                        "[:> Tooltip/Trigger {} [:span \"Default button\"]]\n"
                                        "[:> Tooltip/Trigger {:asChild false} [:span \"Default button\"]]\n"
                                        "[:> Tooltip/Trigger {:asChild enabled?} [:span \"Dynamic\"]]\n"
                                        "[:> Tooltip/Trigger {:asChild false :as-child true} [:span \"Ambiguous\"]]\n"
                                        "[:> Tooltip/Trigger (props) [:span \"Dynamic\"]]\n"
                                        "[:> Other/Trigger {:asChild true} [:span \"Other package\"]]\n"
                                        "[:> Tooltip/TooltipTrigger {:asChild true} [:div \"Details\"]]"))]
    (is (= [[:div 2] [:span 3] [:div 10]] (mapv (juxt :kw :row) findings)))))

(deftest radix-forwards-focus-props-to-the-trigger-child
  (doseq [props ["{:asChild true :tabIndex 0}"
                 "{:as-child true :tab-index -1}"
                 "{:asChild true :tabindex 0}"
                 "{:asChild true :tabIndex focus-order}"
                 "{:asChild true :contentEditable true}"
                 "{:asChild true :contenteditable \"plaintext-only\"}"]]
    (is (empty? (tooltip-findings nil (str "[:> Tooltip/Trigger " props " [:span \"More\"]]")))
        props)))

(deftest radix-trigger-child-props-override-forwarded-props
  (doseq [source ["[:> Tooltip/Trigger {:asChild true :tabIndex 0} [:span {:tab-index nil} \"More\"]]"
                  "[:> Tooltip/Trigger {:as-child true :tab-index 0} [:span {:tabIndex nil} \"More\"]]"
                  "[:> Tooltip/Trigger {:asChild true :contentEditable true} [:div {:content-editable false} \"Edit\"]]"]]
    (is (= 1 (count (tooltip-findings nil source))) source)))

(deftest tooltip-triggers-respect-reader-discard-and-quote
  (let [findings (tooltip-findings tooltip-config
                                   (str "(quote [tooltip {} [:span \"Quoted\"]])\n"
                                        "[#_old :> Tooltip/Trigger {:asChild true} [#_old :div \"Trigger\"]]\n"
                                        "[#_old tooltip {} [#_old :span \"Trigger\"]]"))]
    (is (= [[:div 3] [:span 4]] (mapv (juxt :kw :row) findings)))))

(deftest tooltip-children-with-focus-or-uncertain-props-pass
  (doseq [child ["[:button {:type \"button\"} \"More\"]"
                 "[:a {:href \"/help\"} \"Help\"]"
                 "[:span {:tab-index 0} \"More\"]"
                 "[:span {:tabindex 0} \"Browser attribute\"]"
                 "[:span {:tab-index nil :tabIndex 0} \"One spelling sets tabindex\"]"
                 "[:div {:tabIndex -1} \"Moved by code\"]"
                 "[:span {:tab-index (if active? 0 -1)} \"Roving focus\"]"
                 "[:div {:content-editable true} \"Edit\"]"
                 "[:div {:contentEditable \"plaintext-only\"} \"Edit\"]"
                 "[:span {:contenteditable editable?} \"Editable sometimes\"]"
                 "[:span {:is \"custom-trigger\"} \"Custom\"]"
                 "[:span (merge props {:class \"trigger\"}) \"Unknown props\"]"
                 "[:div props \"Unknown props\"]"
                 "[:span {unknown-key true} \"Unknown key\"]"
                 "[:> ui/button-trigger {:ref trigger} \"Wrapped React button\"]"
                 "[ui/trigger {}]"
                 "(trigger)"]]
    (is (empty? (tooltip-findings tooltip-config (str "[ui/tooltip {} " child "]")))
        child)))

(deftest tooltip-content-editable-values
  (doseq [key [:content-editable :contentEditable :contenteditable]
          [value findings] [["\"false\"" 1] ["false" 1] ["\"true\"" 0] ["true" 0]
                            ["\"\"" 0] ["\"plaintext-only\"" 0] ["editable?" 0]
                            ["(editable?)" 0] ["nil" 1] ["\"invalid\"" 1]]]
    (let [prop (str key " " value)
          child (str "[:span {" prop "} \"More\"]")]
      (is (= findings (count (tooltip-findings tooltip-config (str "[tooltip {} " child "]")))) prop)
      (is (= findings (count (tooltip-findings nil
                                               (str "[:> Tooltip/Trigger {:asChild true " prop "} [:span \"More\"]]"))))
          prop)
      (is (= findings (count (tooltip-findings nil
                                               (str "[:> Tooltip/Trigger {:asChild true " key " true} " child "]"))))
          prop))))

(deftest tooltip-components-must-be-qualified
  (doseq [value ['tooltip #{'tooltip} "ui/tooltip"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":a11y :tooltip-components"
                          (a11y/make-group {:tooltip-components value})))))

(deftest rule->tier-test
  (let [tiers (group/rule->tier a11y/group)]
    (testing "img-alt-missing is a bug"
      (is (= :bugs (get tiers :img-alt-missing))))

    (testing "invalid-tabindex is a bug"
      (is (= :bugs (get tiers :invalid-tabindex))))

    (testing "button-missing-type is a bug"
      (is (= :bugs (get tiers :button-missing-type))))

    (testing "on-click-on-non-interactive is a bug"
      (is (= :bugs (get tiers :on-click-on-non-interactive))))

    (testing "empty-interactive-element is a bug"
      (is (= :bugs (get tiers :empty-interactive-element))))

    (testing "repeated-accessible-name is a bug"
      (is (= :bugs (get tiers :repeated-accessible-name))))

    (testing "aria-live-contradicts-role is a bug"
      (is (= :bugs (get tiers :aria-live-contradicts-role))))))

(deftest suggestions-test
  (let [suggestions (group/suggestions a11y/group)]
    (testing "img-alt-missing suggestion references WCAG"
      (is (re-find #"WCAG.*1\.1\.1" (:img-alt-missing suggestions))))

    (testing "invalid-tabindex suggestion references WCAG focus order"
      (is (re-find #"WCAG.*2\.4\.3" (:invalid-tabindex suggestions))))

    (testing "on-click-on-non-interactive suggestion references WCAG keyboard"
      (is (re-find #"WCAG.*2\.1\.1" (:on-click-on-non-interactive suggestions))))

    (testing "empty-interactive-element suggestion references WCAG name/role/value"
      (is (re-find #"WCAG.*4\.1\.2" (:empty-interactive-element suggestions))))

    (testing "repeated-accessible-name suggestion references WCAG headings and labels"
      (is (re-find #"WCAG.*2\.4\.6" (:repeated-accessible-name suggestions))))

    (testing "aria-live-contradicts-role suggestion references the ARIA implicit-value rule"
      (is (re-find #"Implicit Value for Role" (:aria-live-contradicts-role suggestions))))))

(deftest make-group-label-props-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":label-props must be a keyword or a collection of keywords"
                        (a11y/make-group {:component-aliases {'my.ui/switch {:label-props "label"
                                                                             :tag :switch}}}))
      "string shape is rejected")
  (is (thrown? clojure.lang.ExceptionInfo
               (a11y/make-group {:component-aliases {'my.ui/switch {:label-props ["label"]
                                                                    :tag :switch}}}))
      "collection of strings is rejected")
  (is (some? (a11y/make-group {:component-aliases {'my.ui/switch {:tag :switch}}}))
      "absent label props are none"))

(deftest make-group-alias-key-test
  (is (thrown? clojure.lang.ExceptionInfo
               (a11y/make-group {:component-aliases {'my.ui/button :button
                                                     "my.ui/button" :input}}))
      "same component under two keys is refused")
  (is (thrown? clojure.lang.ExceptionInfo
               (a11y/make-group {:component-aliases {"button" :button}}))
      "string key without package is refused")
  (is (thrown? clojure.lang.ExceptionInfo
               (a11y/make-group {:component-aliases {"@scope/pkg/" :button}}))
      "string key without component is refused")
  (is (some? (a11y/make-group {:component-aliases {"@scope/pkg/Comp" :button}}))
      "scoped package key is accepted"))

(deftest make-group-label-components-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":label-components must be a map"
                        (a11y/make-group {:label-components #{'my.ui/field}}))
      "set of components is refused")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"key must be a fully qualified symbol"
                        (a11y/make-group {:label-components {'field :label}}))
      "unqualified component is refused")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must name a keyword or a collection of keywords"
                        (a11y/make-group {:label-components {'my.ui/field "label"}}))
      "string prop is refused")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must name a keyword or a collection of keywords"
                        (a11y/make-group {:label-components {'my.ui/field #{}}}))
      "empty prop set is refused"))

(deftest make-group-label-styles-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":label-styles must be a collection of fully qualified symbols"
                        (a11y/make-group {:label-styles 'my.ui.styles/field-label}))
      "bare symbol is rejected")
  (is (thrown? clojure.lang.ExceptionInfo
               (a11y/make-group {:label-styles #{'field-label}}))
      "unqualified symbol is rejected")
  (is (thrown? clojure.lang.ExceptionInfo
               (a11y/make-group {:label-styles {'my.ui.styles/field-label true}}))
      "map is rejected")
  (is (some? (a11y/make-group {:label-styles ['my.ui.styles/field-label]}))
      "vector of qualified symbols is accepted"))

(defn- img-snippet
  "Analyze an `[:img …]` vector with no alt as the only form of a project, and return its finding's snippet."
  [img]
  (->> (temp-project/run-group a11y/group {"app/core.cljs" (str "(ns app.core)\n(defn v [] " img ")")})
       :results
       :img-alt-missing
       first
       :form))

(deftest long-form-snippet-is-longest-prefix-that-fits
  (doseq [img [(str "[:img {:src " (apply str (repeat 300 "a")) "}]")
               (str "[:img {:src \"" (apply str (repeat 300 "a")) "\"\n       :title \"x\"}]")
               (str "[:img {:src \"" (apply str (repeat 40 "ab\\\"cd")) "\"\n       :title \"x\"}]")]]
    (let [form (img-snippet img)
          collapsed (str/replace img #"\s+" " ")
          kept (- (count form) 3)]
      (is (str/ends-with? form "...")
          "marks cut with ellipsis")
      (is (= (subs collapsed 0 kept) (subs form 0 kept))
          "keeps prefix of collapsed source")
      (is (<= (count (pr-str form)) 107)
          "fits escaped limit")
      (is (> (count (pr-str (str (subs collapsed 0 (inc kept)) "..."))) 107)
          "keeps longest prefix that fits"))))

(defn- file-source [index]
  (str "(ns views" index " (:require [ui.kit :as kit]))\n"
       (str/join "\n" (for [form (range 20)]
                        (str "(defn field" form " [] [:div [:label {:for \"id" form "\"} \"Name\"]"
                             " [:> kit/Input {:id \"id" form "\"}] [:img {:src \"a.png\"}]"
                             " [:div {:role \"tablist\"} [:> kit/Input {:id \"other" form "\"}]]])")))))

(deftest parallel-run-keeps-file-state-per-file
  (let [dir (io/file (temp-dir/create-dir "cljs-patrol"))
        rule-group (a11y/make-group {:component-aliases {'ui.kit/Input :input}})
        usages (fn [jobs]
                 (binding [*err* (java.io.StringWriter.)]
                   (vec (:usages (parser/analyze-project (disk/typed-path dir) [rule-group] nil jobs)))))]
    (try
      (doseq [index (range 64)]
        (spit (io/file dir (str "views" index ".cljs")) (file-source index)))
      (let [sequential (usages 1)
            parallel (usages 4)]
        (is (= 64 (count (filter #(= :cljs-patrol.groups.a11y/file-literals (:type %)) parallel)))
            "literals collected once per file")
        (is (= sequential parallel)
            "four jobs find what one job finds"))
      (finally
        (temp-dir/delete-dir! dir)))))
