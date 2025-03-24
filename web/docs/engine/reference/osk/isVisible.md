---
title: isVisible
---

## Summary

Return the actual visibility of the On-Screen Keyboard.

## Syntax

```c
keyman.osk.isVisible();
```

### Parameters

None.

### Return Value

`boolean`
:   `true` if the OSK is actually visible, else `false`. Note that this will usually return `false` after any UI event that results in (temporary) loss of focus.
