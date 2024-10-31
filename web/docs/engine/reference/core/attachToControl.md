---
title: attachToControl
---

## Summary

Attach KeymanWeb to HTML element (or IFrame).

## Syntax

```c
keyman.attachToControl(Pelem)
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element to which KeymanWeb will be attached.

### Return Value

`undefined`

## Description

There are two steps involved in enabling input handling with KeymanWeb
for any given control: *attachment* and *enablement*. The attachment
process validates a control's compatibility with KeymanWeb and
establishes special metadata within the system. This data is released if
the control is ever detached. The control will not receive special input
handling until the control is enabled within KeymanWeb.

If `attachToControl()` is called on an element with the `'kmw-disabled'`
class property, it will only establish KeymanWeb's metadata for the
control and initialize the control in a disabled state. A later call to
`enableControl()` may be used to initialize input handling for it.
Otherwise, the control will be both attached and enabled.

When the [initialization property `attachType`](init) is set to
`'auto'`, this function is called automatically for every control on the
page, even those dynamically added. However, when `attachType` is set to
`'manual'`, `attachToControl` must be explicitly called for each control
in need of KeymanWeb's input handling.

## See also:
- [`keyman.detachFromControl()`](detachFromControl)
- [`keyman.enableControl()`](enableControl)
