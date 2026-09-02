---
title: KMManager.getKeyboardFontFilename() (Deprecated)
---

## Summary

(Deprecated) The **`getKeyboardFontFilename()`** method returns the
selected keyboard's font filename.

## Syntax

``` javascript
KMManager.getKeyboardFontFilename()
```

### Returns

Returns the selected keyboard's font filename as `String` if it has any,
empty string otherwise.

## Description

Use this method to get the font filename of the selected keyboard.

Deprecated. Use `getKeyboardTextFontFilename()` instead.

## Examples

### Example: Using `getKeyboardFontFilename()`

The following script illustrate the use of `getKeyboardFontFilename()`:

``` javascript
 String fontFilename = KMManager.getKeyboardFontFilename();
```

## See also

-   [`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
-   [`getKeyboardFontTypeface()` (Deprecated)](getKeyboardFontTypeface)
-   [`getFontTypeface()`](getFontTypeface)
