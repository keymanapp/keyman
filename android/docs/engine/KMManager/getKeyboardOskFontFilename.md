---
title: KMManager.getKeyboardOskFontFilename()
---

## Summary

The **`getKeyboardOskFontFilename()`** method returns the selected
keyboard's OSK font filename and full path.

## Syntax

``` java
KMManager.getKeyboardOskFontFilename()
```

### Returns

Returns the selected keyboard's OSK font filename and full path as
`String` if it has any, empty string otherwise. Note that the
on-screen keyboard will fallback to the keyboard text font if
no OSK font is specified.

The OSK font should not be used for a text view, because some
OSK fonts are appropriate for use only in the on screen keyboard;
see [`&displayMap`](/developer/language/reference/displaymap) for
reference.

## Description

Use this method to get the OSK font filename of the selected keyboard.

## Examples

### Example: Using `getKeyboardOskFontFilename()`

The following script illustrate the use of
`getKeyboardOskFontFilename()`:

``` java
String oskFontFilename = KMManager.getKeyboardOskFontFilename();
```

## See also

-   [`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
-   [`getKeyboardOskFontTypeface()`](getKeyboardOskFontTypeface)
-   [`getKeyboardTextFontTypeface()`](getKeyboardTextFontTypeface)
-   [`getFontTypeface()`](getFontTypeface)
