---
title: insertText (KT)
---

## Summary

Inserts a text string and optional [`deadkey`](/developer/language/reference/deadkey) into the active output element.

## Syntax

```c
keyman.interface.insertText(text, dk);
```

or

```c
KeymanWeb.KT(text, dk); // Shorthand
```

### Parameters

`text`
:   Type: `string`
:   The text to insert.

`dk`
:   Type: `number` *optional*
:   The deadkey's id, if one is to be inserted.

### Return Value

`boolean`
:   `true` if the operation is successful, otherwise `false`.

## Description

This function is designed to allow custom UI elements or custom OSK displays (as for the [EuroLatin keyboard](https://keymanweb.com/#aae,Keyboard_sil_euro_latin)) to directly insert input into a control without losing the current input state due to loss of focus for the control. It is safe for use outside of keyboard code.
