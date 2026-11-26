---
id: CP-0029
title: "anchor-is-valid and jsx-no-script-url"
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

An [:a] with no :href, :href "#" or :href "javascript:…" is a button pretending to be a link: it has no link semantics
and breaks open-in-new-tab. Ports jsx-a11y anchor-is-valid and react jsx-no-script-url.

### Why

Common in older UI code; the fix is a button.

### Flagged

```clojure
[:a {:on-click f} "Edit"]
[:a {:href "#" :on-click f} "Edit"]
```

### Fix

```clojure
[:button {:on-click f :type "button"} "Edit"]
```

### Left alone

```clojure
[:a {:href "/docs"} "Docs"]
```

### Notes

Consider alongside the other anchor checks.
<!-- SECTION:DESCRIPTION:END -->
