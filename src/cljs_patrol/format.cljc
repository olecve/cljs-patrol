(ns cljs-patrol.format
  "Cross-platform printf-style formatting.
  JVM delegates to clojure.core/format; ClojureScript uses goog.string.format,
  which supports the %s / %d / %-Ns specifiers this project uses."
  #?(:cljs (:require goog.string.format
                     [goog.string :as gstring])))

(defn formatf [fmt & args]
  #?(:clj (apply clojure.core/format fmt args)
     :cljs (apply gstring/format fmt args)))
