(ns app.views
  (:require
   [app.util :as util :refer [helper]]
   [re-frame.core :as rf]))

(defn referred [x]
  (helper (helper (helper x))))

(defn aliased [x]
  (util/helper (util/helper (util/helper x))))

(defn macro-ends-chain [x]
  (util/wrap (util/wrap (util/wrap x))))

(defn library-ends-chain [x]
  (rf/subscribe (rf/subscribe (rf/subscribe x))))
