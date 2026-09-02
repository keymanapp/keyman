---
title: KMManager.getKeyboardTextFontFilename()
---

## Summary

The **`getKeyboardTextFontFilename()`** method returns the selected
keyboard's text font filename and full path.

## Syntax

``` java
KMManager.getKeyboardTextFontFilename()
```

### Returns

Returns the selected keyboard's text font filename and full path as
`String` if it has any, empty string otherwise.

## Description

Use this method to get the text font filename of the selected keyboard.

## Examples

### Example: Using `getKeyboardTextFontFilename()`

The following script illustrate the use of
`getKeyboardTextFontFilename()`:

``` java
String textFontFilename = KMManager.getKeyboardTextFontFilename();
```

## See also

-   [`getKeyboardOskFontFilename()`](getKeyboardOskFontFilename)
-   [`getKeyboardTextFontTypeface()`](getKeyboardTextFontTypeface)
-   [`getKeyboardOskFontTypeface()`](getKeyboardOskFontTypeface)
-   [`getFontTypeface()`](getFontTypeface)
