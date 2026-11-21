(ns cljs-patrol.dev.mutate.git
  "Read from git which lines of a file the branch changed, for -s."
  (:require
   [cljs-patrol.dev.mutate.error :as error]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]))

(defn changed-lines
  "Return the line numbers `git diff -U0` output adds or changes in the new version of a file.

  A hunk that only deletes marks the line it deleted after, so the definition that lost the
  lines still counts as changed."
  [diff-text]
  (into #{}
        (mapcat (fn [[_ start length]]
                  (let [start (parse-long start)
                        length (if length (parse-long length) 1)]
                    (if (zero? length)
                      [(max 1 start)]
                      (range start (+ start length))))))
        (re-seq #"(?m)^@@ -\S+ \+(\d+)(?:,(\d+))? @@" diff-text)))

(defn- git [dir & args]
  (apply shell/sh "git" "-C" dir args))

(defn lines-since
  "Return the lines of `file` that differ from where the current branch left git `ref`.

  The diff is taken against the merge base, so commits made on `ref` since then do not count
  as the branch's changes. A file the merge base lacks is new, and every line of it counts."
  [file ref]
  (let [dir (str (.getParentFile (io/file file)))
        path (str (io/file file))]
    (when-not (= "true" (str/trim (:out (git dir "rev-parse" "--is-inside-work-tree"))))
      (throw (error/setup-error (str file " is not in a git work tree, so -s cannot compare it."))))
    (when-not (zero? (:exit (git dir "rev-parse" "--verify" "--quiet" (str ref "^{commit}"))))
      (throw (error/setup-error (str "No git commit named " ref "."))))
    (let [{base-exit :exit
           base-out :out
           base-err :err} (git dir "merge-base" ref "HEAD")
          base (str/trim base-out)]
      (when-not (zero? base-exit)
        (throw (error/setup-error (str "No merge base of " ref " and HEAD; fetch more history. " (str/trim base-err)))))
      (let [tracked (str/trim (:out (git dir "ls-files" "--full-name" path)))
            at-base? (and (seq tracked) (zero? (:exit (git dir "cat-file" "-e" (str base ":" tracked)))))]
        (if at-base?
          (let [{:keys [exit out err]} (git dir "diff" "--no-color" "--no-ext-diff" "-U0" base "--" path)]
            (when-not (zero? exit)
              (throw (error/setup-error (str "git diff against " ref " failed: " (str/trim err)))))
            (changed-lines out))
          :all-lines)))))
