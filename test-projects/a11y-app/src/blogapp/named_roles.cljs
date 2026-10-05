(ns blogapp.named-roles)

(defn ok-img-role-with-aria-label []
  [:svg {:role "img"
         :aria-label "Coverage map"}])

(defn ok-img-role-with-labelledby []
  [:svg {:role "img"
         :aria-labelledby "map-caption"}])

(defn ok-img-role-with-title []
  [:svg {:role "img"
         :title "Coverage map"}])

(defn ok-decorative-icon-hidden []
  ;; the surrounding control owns the name, so the icon opts out entirely
  [:svg {:aria-hidden true}])

(defn ok-dynamic-role [role]
  ;; conservative: non-literal role — skipped
  [:svg {:role role}])

(defn ok-computed-attrs [props]
  ;; conservative: the attrs map is not literal — skipped
  [:svg (build-icon-props props)])

(defn bad-img-role-unnamed []
  [:svg {:role "img"}])

(defn bad-img-role-keyword-spelling []
  [:svg {:role :img}])

(defn bad-img-role-empty-label []
  [:svg {:role "img"
         :aria-label ""}])
