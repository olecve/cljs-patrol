---
id: CP-41
title: 'jsx-no-target-blank: :target "_blank" without :rel noreferrer'
status: Idea
labels: ["a11y"]
created_date: ""
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

A link with :target "_blank" and no :rel holding noreferrer (or noopener) gives the new page window.opener in older
browsers and leaks the referrer. Ports eslint-plugin-react's jsx-no-target-blank. This check can detect literal
target/rel combinations with little ambiguity.

### Why

Security hygiene with near-zero false positives.

### Flagged

```clojure
[:a {:href url :target "_blank"} "Docs"]
```

### Fix

```clojure
[:a {:href url :rel "noreferrer" :target "_blank"} "Docs"]
```

### Left alone

```clojure
[:a {:href url} "Docs"]
[:a {:href url :rel "noopener noreferrer" :target "_blank"} "Docs"]
```

### Notes

Decide whether an internal href (starting with /) is left alone, as ESLint's allowReferrer/enforceDynamicLinks options
do.
<!-- SECTION:DESCRIPTION:END -->
