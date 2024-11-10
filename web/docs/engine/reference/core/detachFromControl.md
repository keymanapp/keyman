---
title: detachFromControl
---

## Summary

Detach KeymanWeb from HTML element (or IFrame).

## Syntax

```c
keyman.detachFromControl(Pelem);
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element for KeymanWeb to detach from, no longer handling input.

### Return Value

`undefined`

## Description

This function entirely detaches KeymanWeb from the specified control,
removing all specialized input handling and data tracking for the
element. As such, detaching effectively 'resets' the control's state
entirely, save for any previously-input text.

For more information about attachment, see [`keyman.attachToControl()`](attachToControl).

Contrast with [`keyman.disableControl()`](disableControl).
