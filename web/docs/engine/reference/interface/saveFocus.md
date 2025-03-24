---
title: saveFocus (KSF)
---

## Summary

Save focus: Temporarily saves keyboard processing data for the currently-focused control.

## Syntax

```c
keyman.interface.saveFocus()
```

or

```c
KeymanWeb.KSF() // Shorthand
```

### Parameters

None.

### Return Value

`undefined`

## Description

Use this function to temporarily preserve all keyboard processing information during a single change-of-focus event.

This function is designed to allow custom UI elements, custom OSK displays (as for the [EuroLatin keyboard](https://keymanweb.com/#aae,Keyboard_sil_euro_latin)), or keyboards with picker-style functionality (such as the [Japanese keyboard](https://keymanweb.com/#jpn,Keyboard_japanese)) to operate without losing the current input state due to loss of focus for the active control.

A good example of when to call this function would be for `onmousedown` events for visual elements not intended to change the element of focus within KeymanWeb. It is safe for use outside of keyboard code.
