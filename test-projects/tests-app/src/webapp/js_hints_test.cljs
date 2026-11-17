(ns webapp.js-hints-test
  (:require [cljs.test :refer [deftest is]]
            [webapp.macros :as m]))

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

(deftest inline-let-value-read
  (let [el ^js (load-canvas)]
    (is (= 1 (.-pendingOp el)))))

(def init-def ^js (load-canvas))

(defn- read-init-def [] (.-pendingOp init-def))

(deftest interop-call-result-read
  (let [v ^js (.-value (load-canvas))]
    (is (= 1 (.-x v)))))

(deftype MutableNode [^js ^:mutable node]
  Object
  (getX [_] (.-pendingOp node)))

(deftest catch-read
  (try
    (load-canvas)
    (catch :default ^js e
      (is (= 1 (.-response e))))))

(deftest thread-last-call-step-read
  (let [^js el (load-canvas)]
    (is (= 1 (->> el (.-pendingOp))))))

(deftest cond-thread-last-call-step-read
  (let [^js el (load-canvas)]
    (is (= 1 (cond->> el true (.-pendingOp))))))

(deftest comment-hint
  (comment
    (let [^js el (load-canvas)]
      (.-pendingOp el))))

(deftest unquoted-read
  (let [^js el (load-canvas)]
    (is (some? `(foo ~(.-pendingOp el))))))

(deftest vector-literal-use
  (let [el (load-canvas)]
    (is (= [el] [^js el]))
    (is (= 1 (.-pendingOp el)))))

(deftest body-vector-not-alias
  (let [^js root (load-canvas)
        a 1]
    [a root]
    (let [a (load-canvas)]
      (is (= 1 (.-pendingOp a))))))

(def ^:dynamic *node* nil)

(deftest dynamic-binding
  (let [^js root (load-canvas)]
    (binding [*node* root]
      (is (= 1 (.-pendingOp *node*))))))

(deftest as-thread-rebound
  (let [^js el (load-canvas)]
    (is (= 1 (as-> el $ (prn $) (.-pendingOp $))))))

(defonce hinted-defonce-value ^js [1 2])

(defn body-vector [x] ^js [x])

(deftest project-doto
  (let [^js el (load-canvas)]
    (is (some? (m/doto el (.focus))))))

(deftest rebound-name-not-read
  (let [^js el (load-canvas)
        c (load-canvas)
        d c
        el 2]
    (is (= 1 (.-pendingOp c)))
    (is (= [d el] [d 2]))))

(deftest doseq-let-modifier-read
  (doseq [x [1]
          :let [^js el (load-canvas x)]]
    (is (= 1 (.-pendingOp el)))))

(deftest letfn-param-read
  (letfn [(f [^js e] (.-type e))]
    (is (= 1 (f 1)))))

(defmulti handle-pair first)

(defmethod handle-pair [:a :b] [^js e] (.-type e))

(deftest discarded-binding-shift
  (let [#_#_a 1
        ^js el (load-canvas)]
    (is (= 1 (.-pendingOp el)))))

(defn- tail-hint [] ^js (load-canvas))

(deftest branch-hints
  (let [el (if (load-canvas) ^js (load-canvas) ^js (load-canvas))]
    (is (= 1 (.-pendingOp el)))))

(deftest as-thread-name-hint
  (is (= 1 (as-> (load-canvas) ^js $ (.-pendingOp $)))))

(deftest thread-into-macro-step
  (let [^js el (load-canvas)]
    (is (some? (-> el (doto (.focus)))))))

(deftest library-let-read
  (m/let [^js el (load-canvas)]
    (is (= 1 (.-pendingOp el)))))

(def doc-def "A canvas." ^js (load-canvas))

(deftest read-with-its-own-map-tag
  (let [^js el (load-canvas)]
    (is (= 1 (.-pendingOp ^{:tag js} el)))))

(deftest read-with-its-own-named-tag
  (let [^js el (load-canvas)]
    (is (= 1 (.-pendingOp ^js/HTMLElement el)))))

(deftest interop-argument-only
  (let [^js end (load-canvas)]
    (is (.valid js/GapCursor end))))

(defn- js-literal-only [^js target]
  #js {:target target})

(defn- let-value-symbol [el]
  (let [x ^js el]
    (is (= 1 (.-pendingOp x)))))

(deftest discarded-before-unread-binding
  (let [#_x ^js el (load-canvas)]
    (is (some? el))))

(deftest catch-unread
  (try
    (load-canvas)
    (catch :default ^js e
      (is (some? e)))))
