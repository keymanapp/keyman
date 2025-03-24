---
title: isEnabled
---

## Summary

Return the user-defined OSK visibility as set by prior calls to [`show`](show) or [`hide`](hide).

## Syntax

```c
keyman.osk.isEnabled();
```

### Parameters

None.

### Return Value

`boolean`
:   `true` if the OSK is enabled, else `false`.

## Description

Actual OSK visibility will also depend on whether or not the target is focused (see isVisible)
