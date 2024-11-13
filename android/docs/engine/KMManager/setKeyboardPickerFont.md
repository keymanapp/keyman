---
title: KMManager.setKeyboardPickerFont()
---

## Summary

The **`setKeyboardPickerFont()`** sets the font for the keyboard picker
menu.

## Syntax

``` javascript
KMManager.setKeyboardPickerFont(Typeface typeface)
```

### Parameters

`typeface`
:   The font

## Description

Use this method to set a font for the keyboard picker menu.

## Examples

### Example: Using `setKeyboardPickerFont()`

The following script illustrate the use of `setKeyboardPickerFont()`:

``` javascript
    KMManager.setKeyboardPickerFont(Typeface.createFromAsset(getAssets(), "fonts/custom_font.ttf"));
```
