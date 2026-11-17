(ns webapp.js-hints-cljc-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer [deftest is]])))

(defn- load-canvas [] #?(:cljs (js/Object.) :clj nil))

(deftest clj-branch-read
  (let [^js el (load-canvas)]
    #?(:clj (.-pendingOp el) :cljs (is (some? el)))))

(deftest cljs-branch-read
  (let [^js el (load-canvas)]
    #?(:clj (is (some? el)) :cljs (is (= 1 (.-pendingOp el))))))

(deftest cljr-branch-read
  (let [^js el (load-canvas)]
    #?(:cljr (.-pendingOp el) :cljs (is (some? el)))))

(deftest refer-all-let-read
  (let [^js el (load-canvas)]
    (is (= 1 (.-pendingOp el)))))

(deftest default-beside-cljs-read
  (let [^js el (load-canvas)]
    #?(:default (.-pendingOp el) :cljs (is (some? el)))))

(deftest default-alone-read
  (let [^js el (load-canvas)]
    #?(:default (is (= 1 (.-pendingOp el))))))

(deftest branch-use-read
  (let [^js el (load-canvas)]
    (is (= 1 (.-pendingOp #?(:cljs el :clj nil))))))

(deftest custom-feature-read
  (let [^js el (load-canvas)]
    #?(:node (is (= 1 (.-pendingOp el))) :cljs (is (some? el)))))

(deftest branch-use-unread
  (let [^js el (load-canvas)]
    (is (some? #?(:cljs el :clj nil)))))
