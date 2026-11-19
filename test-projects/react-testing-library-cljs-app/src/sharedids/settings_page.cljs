(ns sharedids.settings-page
  (:require
   [sharedids.router :as router]))

(defmethod router/page :settings [_]
  [:button {:type :button}
   "Close"])
