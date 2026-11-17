(ns webapp.js-hints-cljc-test
  (:require [cljs.test :refer [deftest is]]))

(defn- load-canvas [] #?(:cljs (js/Object.) :clj nil))

(deftest clj-branch-read
  (let [^js el (load-canvas)]
    #?(:clj (.-pendingOp el) :cljs (is (some? el)))))

(deftest cljs-branch-read
  (let [^js el (load-canvas)]
    #?(:clj (is (some? el)) :cljs (is (= 1 (.-pendingOp el))))))
