(ns cljs-patrol.html-reporter
  "Generates a self-contained HTML report from cljs-patrol analysis results."
  (:require
   [clojure.string :as str]))

(def ^:private css
  (str
   "*{box-sizing:border-box;margin:0;padding:0}"
   "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;"
   "font-size:14px;color:#1a1a1a;padding:24px 40px;background:#f0f2f5;line-height:1.5}"
   "h1{font-size:1.6em;margin-bottom:6px;color:#111}"
   "h2{font-size:1.15em;margin:28px 0 10px;color:#333;text-transform:uppercase;letter-spacing:.05em}"
   "p{color:#666;margin-bottom:20px;font-size:13px}"
   "section{margin-bottom:36px}"
   "table{border-collapse:collapse;width:100%;background:#fff;border-radius:6px;"
   "overflow:hidden;box-shadow:0 1px 4px rgba(0,0,0,.08)}"
   "th,td{padding:9px 14px;text-align:left;border-bottom:1px solid #e8e8e8}"
   "th{background:#f5f5f5;font-weight:600;color:#333}"
   "th[data-sort]{cursor:pointer;user-select:none}"
   "th[data-sort]:hover{background:#ebebeb}"
   "tbody tr:hover{background:#fafafa}"
   "tbody tr:nth-child(even){background:#fafcfd}"
   "tbody tr:nth-child(even):hover{background:#f2f6fa}"
   "table.summary{max-width:480px;margin-bottom:20px}"
   "table.summary td:last-child{text-align:right;font-variant-numeric:tabular-nums;font-weight:600}"
   "table.summary th:last-child{text-align:right}"
   "tr.ok{background:#f0fbf0}"
   "tr.ok:nth-child(even){background:#e8f8e8}"
   "tr.ok:hover{background:#e0f5e0}"
   "tr.warn{background:#fef6f6}"
   "tr.warn:nth-child(even){background:#faeaea}"
   "tr.warn:hover{background:#f5e0e0}"
   "table.issues td:nth-child(1){font-family:'Courier New',monospace;font-size:12px;word-break:break-all}"
   "table.issues td:nth-child(2){font-family:'Courier New',monospace;font-size:11px;color:#555;"
   "max-width:360px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   "table.issues td:nth-child(3){text-align:right;font-variant-numeric:tabular-nums;color:#888}"
   "table.issues th:nth-child(3){text-align:right}"
   "details{background:#fff;border-radius:6px;margin-bottom:8px;"
   "box-shadow:0 1px 4px rgba(0,0,0,.08);overflow:hidden}"
   "details>summary{padding:11px 16px;font-weight:600;cursor:pointer;background:#f8f8f8;"
   "border-bottom:1px solid #e8e8e8;list-style:none;display:flex;align-items:center;gap:8px}"
   "details>summary::-webkit-details-marker{display:none}"
   "details>summary::before{content:'\\25B6';font-size:10px;color:#888;"
   "transition:transform .15s;flex-shrink:0}"
   "details[open]>summary::before{transform:rotate(90deg)}"
   "details>table{border-radius:0;box-shadow:none}"
   "details>.desc{padding:8px 16px 10px;color:#555;font-size:13px;"
   "border-bottom:1px solid #e8e8e8;background:#fff}"))

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
   "rows.forEach(function(r){tbody.appendChild(r);});});});"))

(defn- escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- cell-value [col item]
  (case col
    :keyword (str (:kw item))
    :file (:file item)
    :line (str (:row item))
    :form (str/trim (str (:form item)))))

(defn- col-header [col]
  (case col
    :keyword "Keyword"
    :file "File"
    :line "Line"
    :form "Form"))

(defn- render-details [{:keys [title description columns items]}]
  (let [cnt (count items)
        open-attr (if (pos? cnt) " open" "")]
    (str "<details" open-attr ">\n"
         "<summary>" (escape-html title) " (" cnt ")</summary>\n"
         (when description (str "<p class=\"desc\">" (escape-html description) "</p>\n"))
         "<table class=\"issues\">\n"
         "<thead><tr>"
         (str/join "" (map #(str "<th data-sort>" (escape-html (col-header %)) "</th>") columns))
         "</tr></thead>\n"
         "<tbody>\n"
         (str/join "\n" (map (fn [item]
                               (str "<tr>"
                                    (str/join "" (map #(str "<td>" (escape-html (cell-value % item)) "</td>") columns))
                                    "</tr>"))
                             items))
         "\n</tbody>\n"
         "</table>\n"
         "</details>\n")))

(defn- aggregate-sections [g g-idx run-results]
  (mapv (fn [section]
          {:title (:title section)
           :description (:description section)
           :columns (:columns section)
           :items (vec (mapcat (fn [rr]
                                 ((:data-fn section) (nth (:group-results rr) g-idx)))
                               run-results))})
        (:html-sections g)))

(defn- aggregate-summary [g g-idx run-results]
  (let [per-dir-lines (mapv (fn [rr]
                              ((:summary-lines g) (nth (:group-results rr) g-idx)))
                            run-results)
        first-lines (first per-dir-lines)
        n (count first-lines)]
    (mapv (fn [i]
            (let [label (first (nth first-lines i))
                  total (reduce + (map #(second (nth % i)) per-dir-lines))]
              [label total]))
          (range n))))

(defn- render-summary-table [enabled-groups run-results]
  (let [all-rows (mapcat (fn [g g-idx]
                           (aggregate-summary g g-idx run-results))
                         enabled-groups
                         (range))]
    (str "<table class=\"summary\">\n"
         "<thead><tr><th>Check</th><th>Count</th></tr></thead>\n"
         "<tbody>\n"
         (str/join "\n" (map (fn [[label cnt]]
                               (let [cls (if (zero? cnt) "ok" "warn")]
                                 (str "<tr class=\"" cls "\"><td>" (escape-html (str label))
                                      "</td><td>" cnt "</td></tr>")))
                             all-rows))
         "\n</tbody>\n"
         "</table>\n")))

(defn- render-group-section [g g-idx run-results]
  (let [sections (aggregate-sections g g-idx run-results)]
    (str "<section>\n"
         "<h2>" (escape-html (:name g)) "</h2>\n"
         (str/join "\n" (map render-details sections))
         "</section>\n")))

(defn- render-html [enabled-groups run-results]
  (let [dirs (str/join ", " (map :source-dir run-results))
        timestamp (str (java.time.LocalDateTime/now))]
    (str "<!DOCTYPE html>\n"
         "<html lang=\"en\">\n"
         "<head>\n"
         "<meta charset=\"UTF-8\">\n"
         "<title>cljs-patrol report</title>\n"
         "<style>" css "</style>\n"
         "</head>\n"
         "<body>\n"
         "<h1>cljs-patrol report</h1>\n"
         "<p>Generated: " (escape-html timestamp) " | Analyzed: " (escape-html dirs) "</p>\n"
         "<h2>Summary</h2>\n"
         (render-summary-table enabled-groups run-results)
         (str/join "\n" (map-indexed (fn [i g] (render-group-section g i run-results)) enabled-groups))
         "<script>" js "</script>\n"
         "</body>\n"
         "</html>\n")))

(defn write-report
  "Write a self-contained HTML report to output-path."
  [enabled-groups run-results output-path]
  (spit output-path (render-html enabled-groups run-results)))
