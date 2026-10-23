(ns blogapp.interop
  (:require
   ["@acme/ui-kit" :refer [Button Input Switch TextArea]]
   ["react-resizable-panels" :as rrp :refer [PanelResizeHandle]]))

(defn bad-interop-handle-unnamed []
  [:> PanelResizeHandle {:class "handle"}])

(defn bad-interop-aliased-ns-handle-unnamed []
  [:> rrp/PanelResizeHandle {}])

(defn ok-interop-handle-named []
  [:> PanelResizeHandle {:aria-label "Resize sidebar"}])

(defn bad-interop-switch-unnamed [on?]
  [:> Switch {:checked on?}])

(defn ok-interop-switch-named [on?]
  [:> Switch {:checked on?
              :aria-label "Dark mode"}])

(defn ok-interop-textarea-in-label []
  [:label "Notes" [:> TextArea {}]])

(defn ok-interop-unmapped [props]
  [:> rrp/PanelGroup {:direction "horizontal"} props])

(defn ok-interop-dom-label []
  [:div [:> "label" {:htmlFor "notes"} "Notes"] [:textarea {:id "notes"}]])

(defn ok-interop-switch-in-label []
  [:label "Dark mode" [:> Switch {}]])

(defn ok-interop-button-named-per-item [xs]
  (for [x xs]
    [:> Button {:aria-label (str "Open " x)
                :key x}
     [:img {:src x
            :alt "Open"}]]))

(defn ok-hidden-interop-input-in-button []
  [:button "Go" [:> Input {:type "hidden"}]])

(defn bad-interop-button-holding-button []
  [:>
   Button {:aria-label "Outer"}
   [:div
    [:button "Inner"]]])

(defn bad-button-holding-interop-button []
  [:button "Save" [:> Button {:aria-label "More"}]])
