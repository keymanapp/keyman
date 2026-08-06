---
title: FocusLastActiveElement (deprecated)
---

## Summary

Restore the focus to the text store active before input was moved to KeymanWeb.

Note that the name is misleading: this function move focus to the last active
`TextStore`, not the last active `HTMLElement` (in practice, this is equivalent
in a web browser context).

## Syntax

```js
keyman.interface.FocusLastActiveElement();
keyman.interface.focusLastActiveElement();
KeymanWeb.FocusLastActiveElement();
KeymanWeb.focusLastActiveElement();
```

### Parameters

None.

### Return Value

`undefined`

### Replaced By

This function is deprecated; see instead:

* [`keyman.interface.focusLastActiveTextStore()`](focusLastActiveTextStore)

### See also

* [`keyman.focusLastActiveElement()`](../core/focusLastActiveElement)
