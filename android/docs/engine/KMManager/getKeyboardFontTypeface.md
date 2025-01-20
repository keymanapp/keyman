---
title: KMManager.getKeyboardFontTypeface() (Deprecated)
---

## Summary

(Deprecated) The **`getKeyboardFontTypeface()`** method creates a new
typeface from the selected keyboard's font.

## Syntax

``` javascript
KMManager.getKeyboardFontTypeface(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns the new typeface created from the selected keyboard's font if it
exists, `null` otherwise.

## Description

Use this method to create a new typeface from the selected keyboard's
font if it has any.

## Examples

### Example: Using `getKeyboardFontTypeface()`

The following script illustrate the use of `getKeyboardFontTypeface()`:

``` javascript
    KMTextView textView = (KMTextView) findViewById(R.id.kmTextView);
    Typeface fontTypeface = KMManager.getKeyboardFontTypeface(this);
    textView.setTypeface(fontTypeface);
```

## See also

-   [`getFontTypeface()`](getFontTypeface)
-   [`getKeyboardFontFilename()` (Deprecated)](getKeyboardFontFilename)
