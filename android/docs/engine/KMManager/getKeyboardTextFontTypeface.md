---
title: KMManager.getKeyboardTextFontTypeface()
---

## Summary

The **`getKeyboardTextFontTypeface()`** method creates a new typeface
from the selected keyboard's text font.

## Syntax

``` javascript
KMManager.getKeyboardTextFontTypeface(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns the new typeface created from the selected keyboard's text font
if it exists, `null` otherwise.

## Description

Use this method to create a new typeface from the selected keyboard's
text font if it has any.

## Examples

### Example: Using `getKeyboardTextFontTypeface()`

The following script illustrate the use of
`getKeyboardTextFontTypeface()`:

``` javascript
    KMTextView textView = (KMTextView) findViewById(R.id.kmTextView);
    Typeface textFontTypeface = KMManager.getKeyboardTextFontTypeface(this);
    textView.setTypeface(textFontTypeface);
```

## See also

-   [`getKeyboardOskFontTypeface()`](getKeyboardOskFontTypeface)
-   [`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
-   [`getKeyboardOskFontFilename()`](getKeyboardOskFontFilename)
-   [`getFontTypeface()`](getFontTypeface)
