(ns cljs-patrol.reporters.html
  "Generates a self-contained HTML report from cljs-patrol analysis results."
  (:require
   #?(:clj [cljs-patrol.macros :refer [inline-resource]])
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.emit :as emit]
   [cljs-patrol.format :refer [formatf]]
   [cljs-patrol.fs :as fs]
   [cljs-patrol.group :as group]
   [clojure.string :as str])
  #?(:cljs (:require-macros [cljs-patrol.macros :refer [inline-resource]])))

(def ^:private css (inline-resource "cljs_patrol/report.css"))

(def ^:private js
  (str
   "document.querySelectorAll('th[data-sort]').forEach(function(th){"
   "th.addEventListener('click',function(){"
   "var table=th.closest('table');"
   "var tbody=table.querySelector('tbody');"
   "var col=Array.from(th.parentElement.children).indexOf(th);"
   "var asc=th.getAttribute('data-asc')!=='1';"
   "th.setAttribute('data-asc',asc?'1':'0');"
   "table.querySelectorAll('th').forEach(function(t){"
   "t.textContent=t.textContent.replace(/ [\\u25b2\\u25bc]$/,'');});"
   "th.textContent=th.textContent+(asc?' \\u25b2':' \\u25bc');"
   "var rows=Array.from(tbody.querySelectorAll('tr'));"
   "rows.sort(function(a,b){"
   "var av=a.children[col]?a.children[col].textContent.trim():'';"
   "var bv=b.children[col]?b.children[col].textContent.trim():'';"
   "var an=parseFloat(av),bn=parseFloat(bv);"
   "if(!isNaN(an)&&!isNaN(bn))return asc?an-bn:bn-an;"
   "return asc?av.localeCompare(bv):bv.localeCompare(av);});"
   "rows.forEach(function(r){tbody.appendChild(r);});});});"
   "document.querySelectorAll('[data-action]').forEach(function(b){"
   "b.addEventListener('click',function(){"
   "var open=b.getAttribute('data-action')==='expand-all';"
   "document.querySelectorAll('details').forEach(function(d){d.open=open;});});});"))

(defn- now-str []
  #?(:clj (str (java.time.LocalDateTime/now))
     :cljs (.toISOString (js/Date.))))

(defn- vscode-link [file row]
  (formatf "vscode://file/%s:%d" (fs/absolute-path file) row))

(defn- cell-value [col item]
  (case col
    :keyword (str (:kw item))
    :file [:a {:href (vscode-link (:file item) (:row item))} (:file item)]
    :line (str (:row item))
    :form (str/trim (str (:form item)))))

(defn- col-header [col]
  (case col
    :keyword "Keyword"
    :file "File"
    :line "Line"
    :form "Form"))

(defn- blocking-rule? [fail-on-rules rule-key]
  (and (seq fail-on-rules) (contains? fail-on-rules rule-key)))

(def ^:private url-pattern #"https?://\S+")

(defn- linkify
  "Return a seq of hiccup children for `s` with any http(s) URL replaced by
  an anchor element. Preserves the URL as its own link text. Returns a
  Clojure sequence (not a vector) so the emitter inlines it as siblings."
  [s]
  (when s
    (let [parts (str/split s url-pattern -1)
          urls (re-seq url-pattern s)]
      (loop [ps parts, us urls, acc []]
        (cond
          (empty? ps) (seq acc)
          (empty? us) (seq (conj acc (first ps)))
          :else (recur (rest ps) (rest us)
                       (conj acc (first ps)
                             [:a {:href (first us)
                                  :target "_blank"
                                  :rel "noopener"}
                              (first us)])))))))

(defn- render-summary [{:keys [title cnt blocking? description]}]
  [:summary
   [:span.title title " (" cnt ")"
    (when blocking? [:span.blocking-badge "BLOCKING"])]
   (when (seq description)
     [:span.desc (linkify description)])])

(defn- render-details
  ([section] (render-details section nil))
  ([{:keys [title description columns items rule-key]} fail-on-rules]
   (let [cnt (count items)
         blocking? (blocking-rule? fail-on-rules rule-key)]
     [:details (if (pos? cnt) {:open true} {})
      (render-summary {:title title
                       :cnt cnt
                       :blocking? blocking?
                       :description description})
      [:table.issues
       [:thead
        [:tr (map #(vector :th {:data-sort ""} (col-header %)) columns)]]
       [:tbody
        (map (fn [item]
               [:tr (map #(vector :td (cell-value % item)) columns)])
             items)]]])))

(defn- key->title [k]
  (-> (name k)
      (str/replace #"-" " ")
      str/capitalize))

(defn- infer-columns [item]
  (if (contains? item :form)
    [:form :file :line]
    [:keyword :file :line]))

(defn- aggregate-sections [g g-idx run-results]
  (let [suggs (group/suggestions g)
        first-result (nth (:group-results (first run-results)) g-idx)
        display-keys (keep (fn [[k v]] (when (sequential? v) k)) first-result)]
    (mapv (fn [k]
            (let [all-items (vec (mapcat #(get (nth (:group-results %) g-idx) k) run-results))]
              {:title (key->title k)
               :description (get suggs k "")
               :columns (infer-columns (first all-items))
               :items all-items
               :rule-key k}))
          display-keys)))

(defn- aggregate-summary [g g-idx run-results]
  (let [per-dir-lines (mapv (fn [rr]
                              (group/summary-lines g (nth (:group-results rr) g-idx)))
                            run-results)
        first-lines (first per-dir-lines)
        n (count first-lines)]
    (mapv (fn [i]
            (let [label (first (nth first-lines i))
                  total (->> per-dir-lines
                             (map #(second (nth % i)))
                             (reduce +))]
              [label total]))
          (range n))))

(defn- render-summary-table [enabled-groups run-results]
  (let [all-rows (mapcat (fn [g g-idx]
                           (aggregate-summary g g-idx run-results))
                         enabled-groups
                         (range))]
    [:table.summary
     [:thead [:tr [:th "Check"] [:th "Count"]]]
     [:tbody
      (map (fn [[label cnt]]
             [:tr {:class (if (zero? cnt) "ok" "warn")}
              [:td (str label)]
              [:td cnt]])
           all-rows)]]))

(def ^:private details-toolbar
  [:div.details-toolbar
   [:button {:type "button"
             :data-action "expand-all"} "Expand all"]
   [:button {:type "button"
             :data-action "collapse-all"} "Collapse all"]])

(defn- render-group-section [g g-idx run-results fail-on-rules]
  [:section
   [:h2 (group/group-name g)]
   (map #(render-details % fail-on-rules)
        (aggregate-sections g g-idx run-results))])

(defn- render-html [enabled-groups run-results fail-on-rules]
  (let [dirs (str/join ", " (map :source-dir run-results))
        timestamp (now-str)]
    (emit/emit-document
     [:html {:lang "en"}
      [:head
       [:meta {:charset "UTF-8"}]
       [:title "cljs-patrol report"]
       [:style (emit/raw css)]]
      [:body
       [:h1 "cljs-patrol report"]
       [:p "Generated: " timestamp " | Analyzed: " dirs]
       [:h2 "Summary"]
       (render-summary-table enabled-groups run-results)
       details-toolbar
       (map-indexed (fn [i g] (render-group-section g i run-results fail-on-rules))
                    enabled-groups)
       [:script (emit/raw js)]]])))

(defn write-report
  ([enabled-groups run-results output-path]
   (write-report enabled-groups run-results output-path nil))
  ([enabled-groups run-results output-path fail-on-rules]
   (fs/spit-file output-path (render-html enabled-groups run-results fail-on-rules))))

(defn- render-baseline-details [{:keys [title description columns items rule-key]} new-identities source-dir fail-on-rules]
  (let [cnt (count items)
        blocking? (blocking-rule? fail-on-rules rule-key)]
    [:details (if (pos? cnt) {:open true} {})
     (render-summary {:title title
                      :cnt cnt
                      :blocking? blocking?
                      :description description})
     [:table.issues
      [:thead
       [:tr (map #(vector :th {:data-sort ""} (col-header %)) columns)]]
      [:tbody
       (map (fn [item]
              (let [id (baseline/issue->identity rule-key item source-dir)
                    row-class (if (contains? new-identities id)
                                "new-issue" "baseline-issue")]
                [:tr {:class row-class}
                 (map #(vector :td (cell-value % item)) columns)]))
            items)]]]))

(defn- render-baseline-html
  [enabled-groups run-results new-identities fixed-count fail-on-rules
   blocking-count warning-count]
  (let [dirs (str/join ", " (map :source-dir run-results))
        timestamp (now-str)
        source-dir (:source-dir (first run-results))]
    (emit/emit-document
     [:html {:lang "en"}
      [:head
       [:meta {:charset "UTF-8"}]
       [:title "cljs-patrol report (baseline)"]
       [:style (emit/raw css)]]
      [:body
       [:h1 "cljs-patrol report (baseline)"]
       [:p "Generated: " timestamp " | Analyzed: " dirs]
       (when (seq fail-on-rules)
         [:div.tier-summary
          (str "New: " blocking-count " blocking, " warning-count " warnings.")])
       (when (pos? fixed-count)
         [:div.baseline-banner
          (str fixed-count " baseline issues no longer present - consider running --baseline-write to refresh.")])
       [:h2 "Summary"]
       (render-summary-table enabled-groups run-results)
       details-toolbar
       (map-indexed
        (fn [i g]
          [:section
           [:h2 (group/group-name g)]
           (map #(render-baseline-details % new-identities source-dir fail-on-rules)
                (aggregate-sections g i run-results))])
        enabled-groups)
       [:script (emit/raw js)]]])))

(defn write-baseline-report
  ([enabled-groups run-results output-path new-identities fixed-count]
   (write-baseline-report enabled-groups run-results output-path
                          new-identities fixed-count nil 0 0))
  ([enabled-groups run-results output-path new-identities fixed-count
    fail-on-rules blocking-count warning-count]
   (fs/spit-file output-path
                 (render-baseline-html enabled-groups run-results new-identities fixed-count
                                       fail-on-rules blocking-count warning-count))))
