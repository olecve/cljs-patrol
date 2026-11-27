---
id: CP-0079
title: "Clojure mode: drop experimental from the flag"
status: Idea
labels: ["core", "docs"]
milestone: m-1
created_date: "2026-11-27"
updated_date: ""
references: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

### Problem

The flag and the config key are named `--experimental-clj` and `:experimental-clj`, and the docs call the mode
experimental.

### Why

Once the scope is decided and the rules fit, the name should say the mode is supported.

### Fix

Add `--clj` and `:clj`. Keep `--experimental-clj` and `:experimental-clj` as aliases for at least one release, with a
note in the release notes. Move the docs section out of "Experimental", and update the README and the CI example.

### Order

After CP-0077 and CP-0078.

<!-- SECTION:DESCRIPTION:END -->
