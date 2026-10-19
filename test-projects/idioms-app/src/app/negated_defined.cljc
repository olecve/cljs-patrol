(ns app.negated-defined)

(defmacro when-not [test & body]
  `(if ~test nil (do ~@body)))
