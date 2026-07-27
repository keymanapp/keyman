---
title: setKeyboardForControl
---

## Summary

Associate control with independent keyboard settings initialized to a specific keyboard.

## Syntax

```js
keyman.setKeyboardForControl(elem, keyboard, languageCode);
```

### Parameters

`elem`
:   Type: `Element`
:   The control element to be managed manually.

`keyboard`
:   Type: `string | null` *optional*
:   The ID (internal name) of a keyboard.

`languageCode`
:   Type: `string | null` *optional*
:   The three-letter language code for the keyboard.

### Return Value

`undefined`

## Description

This function establishes the control with separately-managed keyboard
settings from other, non-specialized controls on the page. This may be
undone by setting both `keyboard` and `languageCode` to null,
reverting the control back to default keyboard-management behavior.

Note that either **both** parameters should be set or **neither**. In
particular, if `languageCode` is specified but not `keyboard`, this
method will not automatically select an appropriate keyboard, even if
one has previously been registered with that language code.

Set `keyboard` and `languageCode` to the empty string (`''`) to select the system
keyboard (on non-touch devices), or the first installed keyboard (on touch devices).

Due to system limitations, this function will fail if called on an IFRAME element.

See also [Control-by-Control Example (guide)](../../guide/examples/control-by-control)
