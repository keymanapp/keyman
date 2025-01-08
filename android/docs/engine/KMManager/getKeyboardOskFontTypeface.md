---
title: KMManager.getKeyboardOskFontTypeface()
---

## Summary

The **`getKeyboardOskFontTypeface()`** method creates a new typeface
from the selected keyboard's OSK font.

## Syntax

``` javascript
KMManager.getKeyboardOskFontTypeface(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns the new typeface created from the selected keyboard's OSK font
if it exists, `null` otherwise.

## Description

Use this method to create a new typeface from the selected keyboard's
OSK font if it has any.

## Examples

### Example: Using `getKeyboardOskFontTypeface()`

The following script illustrate the use of
`getKeyboardOskFontTypeface()`:

``` javascript
    Typeface oskFontTypeface = KMManager.getKeyboardOskFontTypeface(this);
```

## See also

-   [`getKeyboardTextFontTypeface()`](getKeyboardTextFontTypeface)
-   [`getKeyboardOskFontFilename()`](getKeyboardOskFontFilename)
-   [`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
-   [`getFontTypeface()`](getFontTypeface)
