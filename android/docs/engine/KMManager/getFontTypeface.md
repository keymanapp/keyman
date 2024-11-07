---
title: KMManager.getFontTypeface()
---

## Summary

The **`getFontTypeface()`** method creates a new typeface from the
specified font filename.

## Syntax

``` javascript
KMManager.getFontTypeface(Context context, String fontFilename)
```

### Parameters

`context`
:   The context.

`fontFilename`
:   The filename of the font.

### Returns

Returns the new typeface created from font file with specified filename
if it exists, `null` otherwise.

## Description

Use this method to create a new typeface from the font file with
specified filename if it exists in `assets/fonts/` folder.

## Examples

### Example: Using `getFontTypeface()`

The following script illustrate the use of `getFontTypeface()`:

``` javascript
    KMTextView textView = (KMTextView) findViewById(R.id.kmTextView);
    Typeface fontTypeface = KMManager.getFontTypeface(this, "aava1.ttf");
    textView.setTypeface(fontTypeface);
```

## See also

-   [`getKeyboardTextFontTypeface()`](getKeyboardTextFontTypeface)
-   [`getKeyboardOskFontTypeface()`](getKeyboardOskFontTypeface)
-   [`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
-   [`getKeyboardOskFontFilename()`](getKeyboardOskFontFilename)
