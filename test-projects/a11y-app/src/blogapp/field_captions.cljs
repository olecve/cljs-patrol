(ns blogapp.field-captions
  (:require
   [blogapp.styles :as styles]
   [blogapp.ui :as ui]))

(defn bad-input-after-label []
  [:div (styles/row)
   [:span (styles/field-label) "Title"]
   [:input {:aria-label "Post title"}]])

(defn bad-label-class-in-props-map []
  [:div
   [:span {:class (styles/field-label)} "Title"]
   [:input {:aria-label "Post title"
            :type "text"}]])

(defn bad-textarea-after-label []
  [:div
   [:p (styles/field-label) "Body"]
   [:textarea {:aria-label "Post body"}]])

(defn bad-select-after-label []
  [:div
   [:span (styles/field-label) "Category"]
   [:select {:aria-label "Post category"}
    [:option "News"]]])

(defn bad-aliased-combobox-after-label [options]
  [:div
   [:span (styles/field-label) "Sort by"]
   [ui/select {:aria-label "Result ordering"
               :options options}]])

(defn bad-role-combobox-after-label []
  [:div
   [:span (styles/field-label) "Author"]
   [:div {:aria-label "Post author"
          :role "combobox"}]])

(defn bad-both-sides-wrapped [value]
  [:div (styles/row)
   [:div (styles/label-group)
    [:p {:class (styles/field-label)} "Published before"]
    [ui/button {:aria-label "Help"}]]
   [:div (styles/field-wrapper)
    [:input {:aria-label "Cut-off date"
             :value value}]]])

(defn ok-aria-labelledby []
  [:div
   [:span {:class (styles/field-label)
           :id "sort-label"}
    "Sort by"]
   [:input {:aria-labelledby "sort-label"}]])

(defn ok-aria-labelledby-beside-aria-label []
  [:div
   [:span (styles/field-label) "Sort by"]
   [:input {:aria-label "Sort by"
            :aria-labelledby "sort-label"}]])

(defn ok-standalone-field []
  [:div
   [:input {:aria-label "Search"}]])

(defn ok-icon-button-after-label [url]
  [:div
   [:span (styles/field-label) "Server URL"]
   [:button {:aria-label "Copy server URL"
             :on-click #(js/navigator.clipboard.writeText url)}
    "Copy"]])

(defn ok-aliased-button-after-label []
  [:div
   [:span (styles/field-label) "Server URL"]
   [ui/button {:aria-label "Copy server URL"}]])

(defn ok-checkbox-after-label []
  [:div
   [:span (styles/field-label) "Notify"]
   [:input {:aria-label "Notify followers"
            :type "checkbox"}]])

(defn ok-computed-input-type [input-type]
  [:div
   [:span (styles/field-label) "Notify"]
   [:input {:aria-label "Notify followers"
            :type input-type}]])

(defn ok-switch-after-label []
  [:div
   [:span (styles/field-label) "Notify"]
   [ui/switch {:aria-label "Notify followers"}]])

(defn ok-unconfigured-style []
  [:div
   [:span (styles/hint) "Title"]
   [:input {:aria-label "Post title"}]])

(defn ok-label-tag []
  [:div
   [:label (styles/field-label) "Title"]
   [:input {:aria-label "Post title"}]])

(defn ok-computed-label-text [title]
  [:div
   [:span (styles/field-label) title]
   [:input {:aria-label "Post title"}]])

(defn ok-blank-label-text []
  [:div
   [:span (styles/field-label) " "]
   [:input {:aria-label "Post title"}]])

(defn ok-label-not-right-before []
  [:div
   [:span (styles/field-label) "Title"]
   [:span "Shown on the front page"]
   [:input {:aria-label "Post title"}]])

(defn ok-label-after-field []
  [:div
   [:input {:aria-label "Post title"}]
   [:span (styles/field-label) "Title"]])

(defn ok-field-not-first-in-wrapper []
  [:div
   [:span (styles/field-label) "Title"]
   [:div
    [:span "Prefix"]
    [:input {:aria-label "Post title"}]]])

(defn ok-component-container [on-close]
  [ui/dialog-root {:on-close on-close}
   [:span (styles/field-label) "Title"]
   [:input {:aria-label "Post title"}]])

(defn ok-discarded-label []
  [:div
   #_[:span (styles/field-label) "Title"]
   [:input {:aria-label "Post title"}]])

(defn ok-props-built-by-call [props]
  [:div
   [:span (styles/field-label) "Title"]
   [:input (merge props {:aria-label "Post title"})]])

(defn bad-field-yielded-by-let [title]
  [:div
   [:span (styles/field-label) "Title"]
   (let [value (str title)]
     [:input {:aria-label "Post title"
              :value value}])])

(defn bad-field-in-branch-inside-wrapper [loaded?]
  [:div (styles/row)
   [:div (styles/field-label) "Body"]
   [:div (styles/value)
    (if loaded?
      [:textarea {:aria-label "Post body"}]
      "Not loaded")]])

(defn bad-field-with-key-metadata [nonce]
  [:div
   [:span (styles/field-label) "Title"]
   ^{:key nonce}
   [:input {:aria-label "Post title"}]])

(defn ok-field-as-call-argument [wrap]
  [:div
   [:span (styles/field-label) "Title"]
   (wrap [:input {:aria-label "Post title"}])])

(defn ok-field-in-when-test [title]
  [:div
   [:span (styles/field-label) "Title"]
   (when [:input {:aria-label "Post title"}]
     title)])

(defn caption [] {:class "caption"})

(defn bad-label-styled-by-own-namespace []
  [:div
   [:span (caption) "Title"]
   [:input {:aria-label "Post title"}]])

(defn bad-label-and-field-two-wrappers-deep []
  [:div
   [:div [:div [:span (styles/field-label) "Title"]]]
   [:div [:div [:input {:aria-label "Post title"}]]]])

(defn ok-label-three-wrappers-deep []
  [:div
   [:div [:div [:div [:span (styles/field-label) "Title"]]]]
   [:input {:aria-label "Post title"}]])

(defn ok-field-three-wrappers-deep []
  [:div
   [:span (styles/field-label) "Title"]
   [:div [:div [:div [:input {:aria-label "Post title"}]]]]])

(defn ok-literal-combobox-tag []
  [:div
   [:span (styles/field-label) "Author"]
   [:combobox {:aria-label "Post author"}]])

(defn ok-field-inside-component [tip]
  [:div
   [:span (styles/field-label) "Title"]
   [ui/dialog-root [:input {:aria-label "Post title"}] tip]])

(defn ok-field-without-aria-label [value]
  [:div
   [:span (styles/field-label) "Title"]
   [:input {:value value}]])

(defn ok-field-role-not-a-field []
  [:div
   [:span (styles/field-label) "Status"]
   [:div {:aria-label "Post status"
          :role "status"}]])

(defn ok-label-names-two-fields []
  [:div
   [:span (styles/field-label) "Date range"]
   [:input {:aria-label "Start date"}]
   [:input {:aria-label "End date"}]])

(defn ok-label-names-wrapped-pair []
  [:div
   [:span (styles/field-label) "Date range"]
   [:div
    [:input {:aria-label "Start date"}]
    [:input {:aria-label "End date"}]]])

(defn bad-field-in-row-beside-label []
  [:div
   [:div
    [:span (styles/field-label) "Name"]
    [:input {:aria-label "Full name"}]]
   [:div
    [:input {:aria-label "Email address"}]]])

(defn bad-flat-form-label-per-field []
  [:div
   [:span (styles/field-label) "Name"]
   [:input {:aria-label "Full name"}]
   [:span (styles/field-label) "Email"]
   [:input {:aria-label "Email address"}]])

(defn ok-empty-list-props []
  [:div
   [:span () "Title"]
   [:input {:aria-label "Post title"}]])

(defn ok-label-style-as-child-vector []
  [:div
   [:span [styles/field-label] "Title"]
   [:input {:aria-label "Post title"}]])

(defn ok-label-names-field-and-unaliased-component []
  [:div
   [:span (styles/field-label) "Date range"]
   [:input {:aria-label "Start date"}]
   [ui/date-field {:aria-label "End date"}]])

(defn ok-label-names-field-and-interop [Input]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "From"}]
   [:> Input {:aria-label "To"}]])

(defn ok-label-names-field-and-component-with-symbol-props [props]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "From"}]
   [ui/text-field props]])

(defn ok-label-names-field-and-component-with-built-props [props]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "From"}]
   [ui/text-field (merge props {:id "to"})]])

(defn ok-label-names-fields-in-a-for [ranges]
  [:div
   [:span (styles/field-label) "Ranges"]
   [:input {:aria-label "First"}]
   (for [r ranges]
     ^{:key r}
     [:input {:aria-label r}])])

(defn ok-label-names-field-yielded-by-when [more?]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "From"}]
   (when more?
     [:input {:aria-label "To"}])])

(defn bad-field-before-plain-component [tip]
  [:div
   [:span (styles/field-label) "Title"]
   [:input {:aria-label "Post title"}]
   [ui/tooltip {:icon "info"} tip]])

(defn bad-field-with-hiccup-in-its-own-props [options]
  [:div
   [:span (styles/field-label) "Author"]
   [ui/select {:aria-label "Post author"
               :option (fn [option] [ui/option {:label option}])
               :options options}]])

(defn ok-label-names-field-and-unread-call [ranges]
  [:div
   [:span (styles/field-label) "Ranges"]
   [:input {:aria-label "First"}]
   (render-range-inputs ranges)])

(defn ok-label-names-fields-built-by-map [emails]
  [:div
   [:span (styles/field-label) "Emails"]
   [:input {:aria-label "Primary"}]
   (doall (map (fn [email] [:input {:aria-label email}]) emails))])

(defn ok-label-names-field-and-symbol [max-input]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   max-input])

(defn ok-label-names-field-and-threaded-field [k]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   (-> [:input {:aria-label "Max"}]
       (with-meta {:key k}))])

(defn ok-label-names-field-and-reader-conditional []
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   #?(:cljs [:input {:aria-label "Max"}])])

(defn ok-label-names-field-and-computed-type-input [input-type]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [:input {:aria-label "Max"
            :type input-type}]])

(defn ok-label-names-field-and-component-with-value [value on-change]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [ui/number-input {:on-change on-change
                     :value value}]])

(defn bad-field-before-non-fields [hint notes note-count]
  [:div
   [:span (styles/field-label) "Title"]
   [:input {:aria-label "Post title"}]
   [:input {:type "checkbox"}]
   "Shown on the front page"
   [:p (styles/hint) hint]
   [:small (str "Up to " 80)]
   [:p (styles/hint) (when hint (str hint))]
   [:span @note-count]
   [ui/tooltip {:content (str "Shown " hint)}]
   #_[:input {:aria-label "Subtitle"}]
   (for [note notes]
     ^{:key note}
     [:p note])])

(defn ok-label-names-field-and-wrapped-symbol [max-input]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [:div (styles/value) max-input]])

(defn ok-label-names-field-and-fields-in-first-slot-for [ranges]
  [:div
   [:span (styles/field-label) "Ranges"]
   [:input {:aria-label "First"}]
   [:div
    (for [r ranges]
      ^{:key r}
      [:input {:aria-label r}])]])

(defn ok-label-names-field-and-field-in-text-branch [more?]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [:span (when more? [:input {:aria-label "Max"}])]])

(defn ok-label-names-field-and-field-in-component-branch [more?]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [ui/card {:gap 2}
    (if more? [:input {:aria-label "Max"}] "No maximum")]])

(defn ok-label-names-field-and-field-in-component-props []
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [ui/form-row {:control [:input {:aria-label "Max"}]}]])

(defn ok-label-names-field-and-field-in-props-items []
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [ui/stack {:children [[:input {:aria-label "Max"}]
                         [:p "Inclusive"]]}]])

(defn ok-label-names-field-and-field-in-render-fn []
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [ui/popover {:content (fn [] [:input {:aria-label "Max"}])}]])

(defn ok-field-yielded-by-look-alike [ready?]
  [:div
   [:span (styles/field-label) "Title"]
   (x/when ready?
     [:input {:aria-label "Post title"}])])

(defn ok-label-names-field-and-deref [max-input]
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   @max-input])

(defn ok-label-names-field-and-field-in-component []
  [:div
   [:span (styles/field-label) "Range"]
   [:input {:aria-label "Min"}]
   [ui/card
    [:input {:aria-label "Max"}]]])
