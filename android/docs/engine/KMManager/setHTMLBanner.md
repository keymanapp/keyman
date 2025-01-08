---
title: KMManager.setHTMLBanner
---

## Summary
The **setHTMLBanner()** method sets the HTML banner content for Keyman Engine to display when suggestions aren't available.

## Syntax

```javascript
KMManager.setHTMLBanner(KeyboardType keyboardType, String content)
```

### Parameters

`keyboardType`
: KeyboardType to be used. `KEYBOARD_TYPE_INAPP` or `KEYBOARD_TYPE_SYSTEM`.

`content`
: HTML content formatted as a string.

### Returns
Returns `true` if HTML banner is set, `false` otherwise.

## Description
When suggestions aren't available for a keyboard, an HTML banner is displayed instead.
Use this method to specify the HTML content to display in the banner to theme your keyboard app.

If the banner theme references assets (like .svg or .css files), ensure you've also called [copyHTMLBannerAssets()](copyHTMLBannerAssets).

Note: The HTML banner needs to be updated whenever the keyboard is reloaded, so call this in `SystemKeyboard.onInitializeInterface(()`.

## Examples

### Example: Using `setHTMLBanner()`

The following script illustrates the use of `setHTMLBanner()`:
```javascript
    String KMGRAY_BANNER = "<div style=\"background: #b4b4b8; width: 100%; height: 100%; position: absolute; left: 0; top: 0\"></div>";
    // Sets the HTML banner content
    KMManager.setHTMLBanner(KeyboardType.KEYBOARD_TYPE_SYSTEM, KMGRAY_BANNER);
```

## See also
* [copyHTMLBannerAssets()](copyHTMLBannerAssets)
