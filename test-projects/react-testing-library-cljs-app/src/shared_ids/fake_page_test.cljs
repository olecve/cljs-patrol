(ns shared-ids.fake-page-test
  (:require
   [shared-ids.router :as router]))

(defmethod router/page :fake [_]
  [:button {:type :button}
   "Close"])
