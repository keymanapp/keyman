---
title: addHotKey
---

## Summary

Add hot key handler to array of document-level hotkeys triggered by key-up event.

## Syntax

```c
keyman.addHotKey(keyCode, shiftState, handler);
```

### Parameters

`keyCode`
:   Type: `number`
:   The base key of the hotkey

`shiftState`
:   Type: `number`
:   `shiftState` is a bitwise combination of SHIFT (0x10), CTRL (0x20) and ALT (0x40).

`handler`
:   Type: `function`
:   The function to be called when the hotkey is triggered. It should not expect any parameters.

### Return Value

`undefined`

## Description

Used to support custom hotkeys within a web document. Only one handler function may exist per hotkey combination.
