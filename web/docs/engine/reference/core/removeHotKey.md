---
title: removeHotKey
---

## Summary

Remove the hotkey handler from document's list of hotkey handlers.

## Syntax

```c
keyman.removeHotkey(keyCode, shiftState);
```

### Parameters

`keyCode`
:   Type: `number`
:   The base key code for the hotkey.

`shiftState`
:   Type: `number`
:   The modifier values corresponding to ALT, CTRL, and SHIFT for the hotkey.

### Return Value

`undefined`

## Description

See also [`keyman.addHotKey()`](addHotKey).
