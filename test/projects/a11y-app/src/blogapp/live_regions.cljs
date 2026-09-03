(ns blogapp.live-regions)

(defn ok-status-with-polite []
  [:div {:role "status"
         :aria-live "polite"}
   "Draft saved"])

(defn ok-alert-with-assertive []
  [:div {:role "alert"
         :aria-live "assertive"}
   "Publish failed"])

(defn ok-log-with-polite []
  [:div {:role "log"
         :aria-live "polite"}
   "Comment posted"])

(defn ok-keyword-spellings []
  [:div {:role :status
         :aria-live :polite}
   "Draft saved"])

(defn ok-status-role-only []
  ;; conformant: the role implies "polite" on its own
  [:div {:role "status"}
   "Draft saved"])

(defn ok-alert-role-only []
  ;; conformant, and the redundant attribute double-speaks in VoiceOver on iOS
  [:div {:role "alert"}
   "Publish failed"])

(defn ok-status-silenced-on-purpose []
  ;; "off" is a deliberate opt-out, not a contradiction
  [:div {:role "status"
         :aria-live "off"}
   "Idle"])

(defn ok-not-a-live-region []
  [:div {:role "navigation"
         :aria-live "polite"}
   "Archive"])

(defn ok-dynamic-role [role]
  ;; conservative: non-literal role — skipped
  [:div {:role role
         :aria-live "assertive"}
   "Draft saved"])

(defn ok-dynamic-aria-live [level]
  ;; conservative: non-literal :aria-live — skipped
  [:div {:role "status"
         :aria-live level}
   "Draft saved"])

(defn ok-computed-aria-live [urgent?]
  ;; conservative: computed :aria-live — skipped
  [:div {:role "status"
         :aria-live (if urgent? "assertive" "polite")}
   "Draft saved"])

(defn bad-alert-downgraded-to-polite []
  [:div {:role "alert"
         :aria-live "polite"}
   "Publish failed"])

(defn bad-status-upgraded-to-assertive []
  [:div {:role "status"
         :aria-live "assertive"}
   "Draft saved"])

(defn bad-log-upgraded-to-assertive []
  [:div {:role "log"
         :aria-live "assertive"}
   "Comment posted"])

(defn bad-keyword-value-mismatch []
  [:div {:role :alert
         :aria-live :polite}
   "Publish failed"])
