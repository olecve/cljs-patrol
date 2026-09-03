(ns blogapp.live-regions)

(defn ok-status-with-polite []
  [:div {:role "status"
         :aria-live "polite"} "Draft saved"])

(defn ok-alert-with-assertive []
  [:div {:role "alert"
         :aria-live "assertive"} "Publish failed"])

(defn ok-log-with-polite []
  [:div {:role "log"
         :aria-live "polite"} "Comment posted"])

(defn ok-keyword-spellings []
  [:div {:role :status
         :aria-live :polite} "Draft saved"])

(defn ok-not-a-live-region []
  [:div {:role "navigation"} "Archive"])

(defn ok-dynamic-role [role]
  ;; conservative: non-literal role — skipped
  [:div {:role role} "Draft saved"])

(defn ok-dynamic-aria-live [level]
  ;; conservative: non-literal :aria-live — skipped
  [:div {:role "status"
         :aria-live level} "Draft saved"])

(defn ok-computed-aria-live [urgent?]
  ;; conservative: computed :aria-live — skipped
  [:div {:role "status"
         :aria-live (if urgent? "assertive" "polite")} "Draft saved"])

(defn bad-status-without-aria-live []
  [:div {:role "status"} "Draft saved"])

(defn bad-alert-without-aria-live []
  [:span {:role "alert"} "Publish failed"])

(defn bad-log-without-aria-live []
  [:div {:role "log"} "Comment posted"])

(defn bad-keyword-role-without-aria-live []
  [:div {:role :status} "Draft saved"])

(defn bad-alert-downgraded-to-polite []
  [:div {:role "alert"
         :aria-live "polite"} "Publish failed"])

(defn bad-status-upgraded-to-assertive []
  [:div {:role "status"
         :aria-live "assertive"} "Draft saved"])

(defn bad-keyword-value-mismatch []
  [:div {:role :alert
         :aria-live :polite} "Publish failed"])

(defn bad-explicit-nil-aria-live []
  [:div {:role "status"
         :aria-live nil} "Draft saved"])
