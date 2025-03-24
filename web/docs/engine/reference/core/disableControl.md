---
title: disableControl
---

## Summary

Disables KeymanWeb input handling for the specified control.

## Syntax

```c
keyman.disableControl(Pelem)
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element to become KeymanWeb-disabled.

### Return Value

`undefined`

## Description

This function is used to explicitly disable KeymanWeb input handling for
a previously-attached control, adding the class `'kmw-disabled'`
property to the element as necessary. It will not, however, detach
KeymanWeb from the control, and any KeymanWeb-specific metadata will
persist until KeymanWeb is explicitly detached from the element.

If `'kmw-disabled'` is added to the control via other means,
`disableControl()` will be called upon it automatically and
non-recursively.

If called on an element in an unattached state, the `'kmw-disabled'` tag will be added to the element.

## See also:
- [`keyman.detachFromControl()`](detachFromControl)
- [`keyman.enableControl()`](enableControl)
