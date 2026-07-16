(ns cljs-patrol.macros
  "Compile-time macros. Kept in .cljc so ClojureScript can pick them up
  through the same namespace name (self-referring)."
  #?(:clj (:require [clojure.java.io :as io])))

#?(:clj
   (defmacro inline-resource
     "Slurp the resource at compile time and embed its contents as a
     string literal. Same behaviour for JVM and CLJS builds — the CLJS
     compiler runs on the JVM, so io/resource has full classpath access."
     [path]
     (slurp (io/resource path))))
