---
title: Manager.fontNameForKeyboard()
---

## Summary

The **`fontNameForKeyboard()`** method returns the font name for the given keyboard.

## Syntax

``` swift
Manager.fontNameForKeyboard(fullID: FullKeyboardID)
```

### Parameter

`fullID`
:   Full ID of the keyboard.

### Returns

Returns `String` of the font name, `nil` if the keyboard doesn't have a
font.

## Description

This syntax can be used for getting the font name of a keyboard.

### Example 1: Using `fontNameForKeyboard()`

The following script illustrates the use of `fontNameForKeyboard()`:

``` swift
let fontName: String?
if let id = Manager.shared.currentKeyboardID {
  fontName = Manager.shared.fontNameForKeyboard(withFullID: id)
} else {
  fontName = nil
}
```
