(ns cljs-patrol.integration.reg-fx-public-fn-test
  (:require
   [cljs-patrol.baseline :as baseline]
   [cljs-patrol.core :as core]
   [cljs-patrol.groups.re-frame :as re-frame]
   [clojure.test :refer [deftest is]]))

(def ^:private findings
  (delay (-> (core/run "test-projects/reg-fx-app/src/webapp" [re-frame/group])
             :group-results
             first
             :reg-fx-public-fn)))

(defn- flagged-keys [] (set (map :kw @findings)))

(deftest reg-fx-public-fn-flags-public-defn-test
  (is (contains? (flagged-keys) :webapp.fx/error)
      "aliased reg-fx with public defn flagged")
  (is (contains? (flagged-keys) :webapp.fx/track)
      "referred reg-fx with public defn flagged")
  (is (contains? (flagged-keys) :webapp.fx/notify)
      "multi-arity defn with docstring flagged")
  (is (contains? (flagged-keys) :webapp.fx/reset)
      "defn after reg-fx flagged")
  (is (contains? (flagged-keys) :webapp.platform/plain)
      "cljc file read")
  (is (contains? (flagged-keys) :webapp.fx/build-request)
      "defn returning map literal flagged, map is body not attr-map")
  (is (= 6 (count @findings))
      "nothing else flagged"))

(deftest reg-fx-public-fn-hint-test
  (is (= "show-error! is public. Inline it into reg-fx and dispatch an event that returns the fx."
         (->> @findings (filter #(= :webapp.fx/error (:kw %))) first :hint))
      "hint names fn"))

(deftest reg-fx-public-fn-leaves-alone-test
  (is (not (contains? (flagged-keys) :webapp.fx/focus))
      "defn- left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/blur))
      "^:private left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/select))
      "^{:private true} left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/alert))
      "inline fn left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/scroll))
      "fn from another namespace left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/log))
      "reg-fx inside defn left alone, its symbol may be a local")
  (is (not (contains? (flagged-keys) :webapp.fx/shared-name))
      "public defn of same name in another file left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/clear))
      "defn inside comment left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/save))
      "reg-event-fx with public defn left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/attr-private))
      "attr-map {:private true} left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/doc-attr-private))
      "attr-map after docstring left alone")
  (is (not (contains? (flagged-keys) :webapp.fx/dropped))
      "defn behind #_ left alone")
  (is (not-any? #{:webapp.fx/no-handler :webapp.fx/short-fn :webapp.fx/partial} (flagged-keys))
      "missing, #() and partial handlers left alone")
  (is (not (contains? (flagged-keys) :webapp.multi-a/shared))
      "file with two ns forms left alone, symbol may come from either")
  (is (not (contains? (flagged-keys) :webapp.in-ns/hello))
      "file with in-ns left alone")
  (is (not (contains? (flagged-keys) :webapp.platform/conditional-defn))
      "defn behind reader conditional left alone")
  (is (not (contains? (flagged-keys) :webapp.platform/conditional-reg))
      "reg-fx behind reader conditional left alone"))

(deftest reg-fx-public-fn-baseline-identity-test
  (is (= {:rule :reg-fx-public-fn
          :key :webapp.fx/error}
         (baseline/issue->identity :reg-fx-public-fn {:kw :webapp.fx/error
                                                      :file "src/webapp/fx.cljs"
                                                      :row 9}))
      "keyed by fx keyword"))
