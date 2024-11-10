---
title: keyman.initialized
---

### Summary

Keymanweb core module initialization state flag.

### Syntax

```js
    keyman.initialized
```

### Type

Integer

### Access

Read only

### Return Value

* `0`, if Keyman Engine for Web is not initialized
* `1`, if Keyman Engine for Web has started initialization
* `2`, if Keyman Engine for Web is completely initialized

### Description

The [`keyman.init()` function](init) is used to initialize Keyman. You can check
this flag to see what the current initialization status is. You should not call
functions other than `keyman.init()` until Keyman initialization is complete. As
`keyman.init()` returns a Promise, the Promise fulfilment callback is the
appropriate place to perform post-init steps.

### History

* 2.0: Keyman Engine for Web supports values `0`, `1`, or `2`.
* 16.0: Documentation updated to match implementation.
