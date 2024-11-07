---
title: KMManager.getKeyboardOskFontFilename()
---

## Summary

The **`getKeyboardOskFontFilename()`** method returns the selected
keyboard's OSK font filename.

## Syntax

``` javascript
KMManager.getKeyboardOskFontFilename()
```

### Returns

Returns the selected keyboard's OSK font filename as `String` if it has
any, empty string otherwise.

## Description

Use this method to get the OSK font filename of the selected keyboard.

## Examples

### Example: Using `getKeyboardOskFontFilename()`

The following script illustrate the use of
`getKeyboardOskFontFilename()`:

``` javascript
 String oskFontFilename = KMManager.getKeyboardOskFontFilename();
```

## See also

-   [`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
-   [`getKeyboardOskFontTypeface()`](getKeyboardOskFontTypeface)
-   [`getKeyboardTextFontTypeface()`](getKeyboardTextFontTypeface)
-   [`getFontTypeface()`](getFontTypeface)
