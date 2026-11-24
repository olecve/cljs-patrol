(ns cljs-patrol.disk
  "Paths on the real disk for tests.

  They come from NIO and not from `cljs-patrol.fs`, because a mutant can change `fs`."
  (:import
   [java.nio.file Path]))

(defn path
  "Make a `Path` on the default file system from the text of `first-part` and of each of `more`."
  ^Path [first-part & more]
  (Path/of (str first-part) (into-array String (map str more))))

(defn typed-path
  "Make a {:path :text} map, as `core/run-all` takes for a source folder, with the text of the `Path` as its typed text."
  [first-part & more]
  (let [p (apply path first-part more)]
    {:path p
     :text (str p)}))
