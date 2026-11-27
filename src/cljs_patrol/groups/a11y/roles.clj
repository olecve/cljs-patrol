(ns cljs-patrol.groups.a11y.roles)

(def valid-roles
  "Non-abstract role tokens from WAI-ARIA 1.2, DPUB-ARIA 1.0 and Graphics-ARIA 1.0.
  Keep the canonical spellings here so checks of roles and their properties share one vocabulary."
  #{"alert" "alertdialog" "application" "article" "banner" "blockquote" "button"
    "caption" "cell" "checkbox" "code" "columnheader" "combobox" "complementary"
    "contentinfo" "definition" "deletion" "dialog" "directory" "document" "emphasis"
    "feed" "figure" "form" "generic" "grid" "gridcell" "group" "heading" "img"
    "insertion" "link" "list" "listbox" "listitem" "log" "main" "marquee" "math"
    "menu" "menubar" "menuitem" "menuitemcheckbox" "menuitemradio" "meter" "navigation"
    "none" "note" "option" "paragraph" "presentation" "progressbar" "radio" "radiogroup"
    "region" "row" "rowgroup" "rowheader" "scrollbar" "search" "searchbox" "separator"
    "slider" "spinbutton" "status" "strong" "subscript" "superscript" "switch" "tab"
    "table" "tablist" "tabpanel" "term" "textbox" "time" "timer" "toolbar" "tooltip"
    "tree" "treegrid" "treeitem"
    "doc-abstract" "doc-acknowledgments" "doc-afterword" "doc-appendix" "doc-backlink"
    "doc-biblioentry" "doc-bibliography" "doc-biblioref" "doc-chapter" "doc-colophon"
    "doc-conclusion" "doc-cover" "doc-credit" "doc-credits" "doc-dedication" "doc-endnote"
    "doc-endnotes" "doc-epigraph" "doc-epilogue" "doc-errata" "doc-example" "doc-footnote"
    "doc-foreword" "doc-glossary" "doc-glossref" "doc-index" "doc-introduction" "doc-noteref"
    "doc-notice" "doc-pagebreak" "doc-pagelist" "doc-part" "doc-preface" "doc-prologue"
    "doc-pullquote" "doc-qna" "doc-subtitle" "doc-tip" "doc-toc"
    "graphics-document" "graphics-object" "graphics-symbol"})
