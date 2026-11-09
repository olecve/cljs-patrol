(ns webapp.test-id-test
  (:require
   [cljs.test :refer [deftest is]]
   [react-testing-library-cljs.screen :as s]
   [react-testing-library-cljs.within :as within]))

(deftest finds-send-button
  (.click (s/get-by-test-id "send-button")))

(deftest finds-save-button
  (.click (s/query-by-test-id "save-button")))

(deftest finds-save-button-within
  (.click (within/get-by-test-id (s/get-by-role "main") "save-button")))

(deftest finds-home-link
  (.click (s/get-by-test-id "home-link")))

(deftest finds-edit-area
  (.click (s/get-by-test-id "edit-area")))

(deftest finds-agree-box
  (.click (s/get-by-test-id "agree-box")))

(deftest finds-query-input
  (.click (s/get-by-test-id "query-input")))

(deftest finds-find-input
  (.click (s/get-by-test-id "find-input")))

(deftest finds-pick-input
  (.click (s/get-by-test-id "pick-input")))

(deftest finds-go-input
  (.click (s/get-by-test-id "go-input")))

(deftest finds-country-select
  (.click (s/get-by-test-id "country-select")))

(deftest finds-tags-select
  (.click (s/get-by-test-id "tags-select")))

(deftest finds-sized-select
  (.click (s/get-by-test-id "sized-select")))

(deftest finds-settings-dialog
  (.click (s/get-by-test-id "settings-dialog")))

(deftest finds-all-picked-buttons
  (run! #(.click %) (s/get-all-by-test-id "picked-button")))

(deftest finds-content-scroll
  (.click (s/get-by-test-id "content-scroll")))

(deftest finds-card-by-built-id
  (.click (s/get-by-test-id (str "card-" 1))))

(deftest finds-undeclared-id
  (.click (s/get-by-test-id "banner-icon")))

(deftest finds-shared-id
  (.click (s/get-by-test-id "status-badge")))

(deftest finds-component-button
  (.click (s/get-by-test-id "component-button")))

(deftest finds-hidden-button
  (.click (s/get-by-test-id "hidden-button")))

(deftest finds-inner-button
  (.click (s/get-by-test-id "inner-button")))

(deftest finds-icon-only
  (.click (s/get-by-test-id "icon-only")))

(deftest finds-symbol-label
  (.click (s/get-by-test-id "symbol-label")))

(deftest finds-hidden-text
  (.click (s/get-by-test-id "hidden-text")))

(deftest finds-bare-input
  (.click (s/get-by-test-id "bare-input")))

(deftest finds-file-input
  (.click (s/get-by-test-id "file-input")))

(deftest finds-kind-input
  (.click (s/get-by-test-id "kind-input")))

(deftest finds-no-href-link
  (.click (s/get-by-test-id "no-href-link")))

(deftest finds-decor-div
  (.click (s/get-by-test-id "decor-div")))

(deftest finds-mystery-div
  (.click (s/get-by-test-id "mystery-div")))

(deftest finds-const-button
  (.click (s/get-by-test-id "const-button")))

(deftest finds-merged-button
  (.click (s/get-by-test-id "merged-button")))

(deftest finds-with-rebound-lookup
  (let [s/get-by-test-id identity]
    (.click (s/get-by-test-id "save-button"))))

(deftest finds-send-button-past-discarded-id
  (.click (s/get-by-test-id #_"old-send" "send-button")))

(deftest finds-by-symbol-named-like-id
  (let [send-button "other"]
    (.click (s/get-by-test-id send-button))))

(deftest finds-by-label-text
  (.click (s/get-by-label-text "labelled-button")))

(deftest finds-listed-box
  (.click (s/get-by-test-id "listed-box")))

(deftest finds-blank-text
  (.click (s/get-by-test-id "blank-text")))

(deftest finds-text-dialog
  (.click (s/get-by-test-id "text-dialog")))

(deftest finds-label-as-id
  (.click (s/get-by-test-id "label-as-id")))

(deftest finds-dual-role
  (.click (s/get-by-test-id "dual-role")))

(deftest finds-third-map
  (.click (s/get-by-test-id "third-map")))

(deftest finds-custom-element
  (.click (s/get-by-test-id "custom-element")))

(deftest finds-odd-key
  (.click (s/get-by-test-id "odd-key")))

(deftest finds-svg-button
  (.click (s/get-by-test-id "svg-button")))

(deftest finds-class-button
  (.click (s/get-by-test-id "class-button")))

(deftest finds-img-button
  (.click (s/get-by-test-id "img-button")))

(deftest finds-comp-text
  (.click (s/get-by-test-id "comp-text")))

(deftest finds-fn-text
  (.click (s/get-by-test-id "fn-text")))

(deftest finds-case-button
  (.click (s/get-by-test-id "case-button")))

(deftest finds-when-text
  (.click (s/get-by-test-id "when-text")))

(deftest finds-multi-var
  (.click (s/get-by-test-id "multi-var")))

(deftest finds-href-var
  (.click (s/get-by-test-id "href-var")))

(deftest finds-empty-href
  (.click (s/get-by-test-id "empty-href")))

(deftest finds-empty-list
  (.click (s/get-by-test-id "empty-list")))

(deftest finds-list-var
  (.click (s/get-by-test-id "list-var")))

(deftest finds-aria-hidden-var
  (.click (s/get-by-test-id "aria-hidden-var")))

(deftest finds-aria-hidden-anc
  (.click (s/get-by-test-id "aria-hidden-anc")))

(deftest finds-style-hidden
  (.click (s/get-by-test-id "style-hidden")))

(deftest finds-vis-parent
  (.click (s/get-by-test-id "vis-parent")))

(deftest finds-style-child
  (.click (s/get-by-test-id "style-child")))

(deftest finds-style-var
  (.click (s/get-by-test-id "style-var")))

(deftest finds-kw-collide
  (.click (s/get-by-test-id "kw-collide")))

(deftest finds-str-collide
  (.click (s/get-by-test-id "str-collide")))

(deftest finds-ws-collide
  (.click (s/get-by-test-id "ws-collide")))

(deftest finds-labelledby-only
  (.click (s/get-by-test-id "labelledby-only")))

(deftest finds-blank-label
  (.click (s/get-by-test-id "blank-label")))

(deftest finds-image-value
  (.click (s/get-by-test-id "image-value")))

(deftest finds-text-role-button
  (.click (s/get-by-test-id "text-role-button")))

(deftest finds-hidden-role
  (.click (s/get-by-test-id "hidden-role")))

(deftest finds-size-one
  (.click (s/get-by-test-id "size-one")))

(deftest finds-plain-in-span
  (.click (s/get-by-test-id "plain-in-span")))

(deftest finds-exact-with-options
  (run! #(.click %) (s/query-all-by-test-id "exact" {:exact false})))

(deftest finds-in-dynamic-div
  (.click (s/get-by-test-id "in-dynamic-div")))

(deftest finds-inner-space
  (.click (s/get-by-test-id "inner space")))

(deftest finds-hidden-false
  (.click (s/get-by-test-id "hidden-false")))

(deftest finds-nil-href
  (.click (s/get-by-test-id "nil-href")))

(deftest finds-list-nil
  (.click (s/get-by-test-id "list-nil")))

(deftest finds-string-style
  (.click (s/get-by-test-id "string-style")))

(deftest finds-in-component
  (.click (s/get-by-test-id "in-component")))

(deftest finds-label-sym
  (.click (s/get-by-test-id "label-sym")))

(deftest finds-titled
  (.click (s/get-by-test-id "titled")))

(deftest finds-button-value
  (.click (s/get-by-test-id "button-value")))

(deftest finds-no-value-button
  (.click (s/get-by-test-id "no-value-button")))

(deftest finds-built-key-free
  (.click (s/get-by-test-id "built-key-free")))

(deftest finds-call-built-zz
  (.click (s/get-by-test-id "call-built-zz")))

(deftest finds-pre-one
  (.click (s/get-by-test-id "pre-one")))

(deftest finds-x-tail
  (.click (s/get-by-test-id "x-tail")))

(deftest finds-other-one
  (.click (s/get-by-test-id "other-one")))

(deftest finds-kw-elsewhere
  (.click (s/get-by-test-id "kw-elsewhere")))

(deftest finds-save-top
  (.click (s/get-by-test-id "save-top")))

(deftest finds-delete-all
  (.click (s/get-by-test-id "delete-all")))

(deftest finds-under-param-attrs
  (.click (s/get-by-test-id "under-param-attrs")))

(deftest finds-under-let-attrs
  (.click (s/get-by-test-id "under-let-attrs")))

(deftest finds-in-closed-dialog
  (.click (s/get-by-test-id "in-closed-dialog")))

(deftest finds-in-open-dialog
  (.click (s/get-by-test-id "in-open-dialog")))

(deftest finds-in-template
  (.click (s/get-by-test-id "in-template")))

(deftest finds-role-empty
  (.click (s/get-by-test-id "role-empty")))

(deftest finds-size-zero
  (.click (s/get-by-test-id "size-zero")))

(deftest finds-multiple-zero
  (.click (s/get-by-test-id "multiple-zero")))

(deftest finds-href-true
  (.click (s/get-by-test-id "href-true")))

(deftest finds-padded-none
  (.click (s/get-by-test-id "padded-none")))

(deftest finds-upper-none
  (.click (s/get-by-test-id "upper-none")))

(deftest finds-menu-titled
  (.click (s/get-by-test-id "menu-titled")))

(deftest finds-item-1
  (.click (s/get-by-test-id "item-1")))

(deftest finds-sym-id
  (.click (s/get-by-test-id "sym-id")))

(deftest finds-threaded-button
  (.click (s/get-by-test-id "qq7zz9")))

(deftest finds-joined-part
  (.click (s/get-by-test-id "yy5~ww3")))

(deftest finds-mid-a-dle-b-end
  (.click (s/get-by-test-id "mid-a-dle-b-end")))

(deftest finds-mid-a-end
  (.click (s/get-by-test-id "mid-a-end")))

(deftest finds-only-discarded
  (.click (s/get-by-test-id "only-discarded")))

(deftest finds-only-in-comment
  (.click (s/get-by-test-id "only-in-comment")))

(deftest finds-icon-and-text
  (.click (s/get-by-test-id "icon-and-text")))

(deftest finds-hidden-nil
  (.click (s/get-by-test-id "hidden-nil")))

(deftest finds-with-break
  (.click (s/get-by-test-id "with-break")))

(deftest finds-display-var
  (.click (s/get-by-test-id "display-var")))

(deftest finds-visibility-var
  (.click (s/get-by-test-id "visibility-var")))

(deftest finds-uneval-child
  (.click (s/get-by-test-id "uneval-child")))

(deftest finds-bound-id
  (.click (s/get-by-test-id "bound-id")))

(deftest finds-single-zz-more
  (.click (s/get-by-test-id "single-zz-more")))
