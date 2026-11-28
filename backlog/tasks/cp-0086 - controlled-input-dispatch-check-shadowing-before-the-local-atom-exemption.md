---
id: CP-0086
title: "controlled-input-dispatch: check shadowing before the local-atom exemption"
status: Idea
labels: ["reagent"]
created_date: "2026-11-28"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

`controlled-input-dispatch` leaves an input alone when its value comes from a component-local Reagent atom. With
`[reagent.core :refer [atom]]`, a local binding named `atom` can shadow Reagent's, and the exemption still applies, so a
value that is not local state is missed.

### Flagged

```clojure
;; missed today: atom here is the local identity, not Reagent's
(ns app.view (:require [reagent.core :refer [atom]]))

(defn editor [text]
  (let [atom identity
        draft (atom text)]
    [:input {:value @draft
             :on-change #(rf/dispatch [::set-text %])}]))
```

### Fix

Check whether the bare head of the initializer is locally bound before granting the exemption, as the dispatch check
already does.

### Left alone

`(r/atom text)` and an unshadowed referred `atom` keep the exemption.

<!-- SECTION:DESCRIPTION:END -->
