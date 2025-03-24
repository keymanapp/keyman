---
title: getKeyboardForControl
---

## Summary

Obtain the keyboard set for a specific control, if it exists.

## Syntax

```c
keyman.getKeyboardForControl(Pelem);
```

### Parameters

`Pelem`
:   Type: `Element`
:   An HTML input control.

### Return Value

`string|null`
:   If the control has independent keyboard settings, returns the ID (internal name) of a keyboard. Otherwise, returns `null`.

## Description

This function is useful for determining when a control has
independently-managed keyboard settings within KeymanWeb. **Note** that it
will return `null` otherwise, even if there is an active keyboard within
KeymanWeb.

## See also
-  [`keyman.setKeyboardForControl()`](setKeyboardForControl) contrast with [`keyman.getActiveKeyboard()`](getActiveKeyboard)
