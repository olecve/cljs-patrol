(ns blogapp.gallery)

(defn ok-with-alt []
  [:img {:src "/img/logo.png"
         :alt "Blogapp logo"}])

(defn ok-decorative []
  [:img {:src "/img/divider.svg"
         :alt ""}])

(defn ok-hiccup-class-with-alt []
  [:img.hero {:src "/img/hero.jpg"
              :alt "Author at the beach"}])

(defn ok-hiccup-id-with-alt []
  [:img#avatar {:src "/img/avatar.png"
                :alt "User avatar"}])

(defn ok-quoted-vector-skipped []
  ;; a quoted vector is test data, not code — must not fire the rule
  '[:img])

(defn ok-meta-wrapped-attrs []
  ;; meta on attrs makes them non-literal to us — treated as :dynamic, skipped
  [:img ^:foo {:src "/img/x.png"}])

(defn bad-no-attrs []
  [:img])

(defn bad-attrs-without-alt []
  [:img {:src "/img/thumb.jpg"}])

(defn bad-hiccup-class-without-alt []
  [:img.thumb {:src "/img/thumb.jpg"
               :width 120}])

(defn bad-empty-attrs []
  [:img {}])

(defn bad-two-on-same-line []
  ;; two missing-alt siblings on the same line must yield two findings, not
  ;; one — baseline identity includes :col for exactly this case
  [:span [:img {:src "/a.png"}] [:img {:src "/b.png"}]])

(defn ok-namespaced-alt-key-does-not-satisfy []
  ;; ::alt is not the HTML :alt attribute, so this img is still flagged
  [:img {::alt "wrong"}])

(defn dynamic-attrs-skipped [props]
  ;; conservative: attrs are a non-literal form, don't flag either way
  [:img (merge {:src "/img/x.png"} props)])
