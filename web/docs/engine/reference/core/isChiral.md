---
title: isChiral
---

## Summary

Test if a given keyboard recognizes chiral modifier data, such as
left-control vs right-control.

## Syntax

```c
keyman.isChiral(keyboard);
```

### Parameters

`keyboard`
:   Type: `string` *optional*
:   The id (identifying name) of a keyboard. Defaults to the currently-active keyboard if not specified.

### Return Value

`boolean`
:   `true` if the keyboard accepts chiral (left-vs-right) variants of CTRL and ALT, `false` if not.

## Description

The specified keyboard must have been fully loaded at some point for
this information to be accessible; otherwise it will return `false`
(non-chiral). It is always accurate for the currently-active keyboard.

With the present system architecture, this is a bit complicated in that
a keyboard is only set to 'active' after the
[`keyboardloaded`](../events/keyman/keyboardloaded) event completes and
after the [`keyboardchange`](../events/keyman/keyboardchange) event has
completed for the first activation of a keyboard. However, both events
return the name of the keyboard involved in the event, and this can be
passed as an argument to `isChiral` from the
[`keyboardloaded`](../events/keyman/keyboardloaded) event's handler to
obtain the correct keyboard chirality information.
