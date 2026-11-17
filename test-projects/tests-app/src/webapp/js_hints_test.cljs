(ns webapp.js-hints-test
  (:require [cljs.test :refer [deftest is]]))

(defn- load-canvas [] (js/Object.))

(def ^:private ^js stacked-def (js/Object.))

(defn ^:private ^js stacked-defn [] (js/Object.))

(defn multi-arity
  (^js [] (js/Object.))
  (^js [x] x))

(defmulti handle-unread-return identity)

(defmethod handle-unread-return :k ^js [x] x)

(defn- inline-fn-return [p]
  (.then p (fn ^js [x] x)))

(def reify-fake
  (reify Object
    (handleEvent [_ ^js e] (.-type e))))

(deftype FakeNode [^js node]
  Object
  (getX [_] (.-x node)))

(deftest hint-on-interop-call
  (let [v ^js (.-value (load-canvas))]
    (is (= 1 v))))

(deftest thread-last-read
  (let [^js el (load-canvas)]
    (is (= 1 (->> el .-x)))))

(deftest some-thread-read
  (let [^js el (load-canvas)]
    (is (= 1 (some-> el .-x)))))

(deftest cond-thread-read
  (let [^js el (load-canvas)]
    (is (some? (cond-> el true (.focus))))))

(deftest as-thread-read
  (let [^js el (load-canvas)]
    (is (= 1 (as-> el $ (.-x $))))))

(deftest doto-later-step
  (let [^js el (load-canvas)]
    (is (some? (doto el (prn) (.focus))))))

(deftest alias-read
  (let [^js root (load-canvas)
        node root]
    (is (= 1 (.-x node)))))

(deftest inline-arg-hint
  (let [el (load-canvas)]
    (is (= 1 (count ^js el)))
    (is (= 1 (.-x el)))))

(deftest read-only-when-discarded
  (let [^js el (load-canvas)]
    #_(.-x el)
    (is (= 1 (count el)))))

(deftest read-only-in-comment
  (let [^js el (load-canvas)]
    (comment (.-x el))
    (is (= 1 (count el)))))

(deftest namespaced-namesake
  (let [^js el (load-canvas)]
    (is (= 1 (.-x other/el)))
    (is (= 1 (count el)))))

(deftest as-thread-inline-read
  (is (= 1 (as-> ^js (load-canvas) $ (.-x $)))))

(deftest cond-thread-last-read
  (let [^js el (load-canvas)]
    (is (= 1 (cond->> el true .-x)))))

(def hinted-vector-value ^js [1 2])

(deftest destructured-read
  (let [{:keys [^js panel]} {:panel (load-canvas)}]
    (is (= 1 (.-x panel)))))
