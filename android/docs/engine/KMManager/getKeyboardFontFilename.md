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

## Examples

### Example: Using `getKeyboardFontFilename()`

The following script illustrate the use of `getKeyboardFontFilename()`:

``` javascript
 String fontFilename = KMManager.getKeyboardFontFilename();
```

## See also

-   [`getKeyboardFontTypeface()` (Deprecated)](getKeyboardFontTypeface)
-   [`getFontTypeface()`](getFontTypeface)
