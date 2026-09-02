---
title: GetLastActiveElement (deprecated)
---

## Summary

Returns the last active target text store before the current KMW operation.

Note that the name is misleading: this function returns the last active
`TextStore`, not the last active `HTMLElement`. Use instead
[`keyman.interface.getLastActiveTextStore()`](getLastActiveTextStore).

## Syntax

```js
keyman.interface.GetLastActiveElement();
keyman.interface.getLastActiveElement();
KeymanWeb.GetLastActiveElement();
KeymanWeb.getLastActiveElement();
```

### Parameters

None.

### Return Value

`TextStore`
:   The requested text store.

### Replaced By

This function is deprecated; see instead:

* [`keyman.interface.getLastActiveTextStore()`](getLastActiveTextStore)

### See also

* [`keyman.getLastActiveElement()`](../core/getLastActiveElement)
