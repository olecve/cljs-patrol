(ns blogapp.unrendered)

(defn discarded-markup []
  ;; #_ discards the vector outright, so nothing in it reaches the DOM
  [:div
   #_[:img {:src "/hero.png"}]
   #_[:span {:tab-index 5}]
   "Shown"])

(defn quoted-markup [render]
  ;; quoted hiccup is data: the nested vectors are not rendered either
  [:div
   (render '[:div [:img {:src "/hero.png"}] [:span {:tab-index 5}]])])

(defn syntax-quoted-markup [expand]
  [:div
   (expand `[:div [:img {:src "/hero.png"}]])])
