---
title: enableControl
---

## Summary

Enables KeymanWeb input handling for the specified control.

## Syntax

```c
keyman.enableControl(Pelem)
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element to become KeymanWeb-enabled.

### Return Value

`undefined`

## Description

There are two steps involved in enabling input handling with KeymanWeb
for any given control: *attachment* and *enablement*. The attachment
process validates a control's compatibility with KeymanWeb and
establishes special metadata within the system. This data is released if
the control is ever detached. The control will not receive special input
handling unless the control is enabled within KeymanWeb.

This function is used to explicitly enable KeymanWeb input handling for
a previously-attached control, removing the class `'kmw-disabled'`
property from the element as necessary. It will not, however, attach
KeymanWeb to the control.

If `'kmw-disabled'` is removed from the control via other means, `enableControl()` will be called upon it automatically and non-recursively.

If called on an element in an unattached state, the `'kmw-disabled'` tag will be removed from the element.

## See also 
- [`keyman.attachToControl()`](attachToControl)
- [`keyman.disableControl()`](disableControl)
