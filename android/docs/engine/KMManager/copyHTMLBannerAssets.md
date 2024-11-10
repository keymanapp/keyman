---
title: KMManager.copyHTMLBannerAssets
---

## Summary
The **copyHTMLBannerAssets()** method copies a folder of HTML banner assets so it's available for your keyboard app's resources.

## Syntax

```javascript
KMManager.copyHTMLBannerAssets(Context context, String path)
```

### Parameters

`context`
: The context.

`path`
: Relative to the /assets folder, the folder which contains the HTML assets to display when suggestions aren't available.

### Returns
Returns `true` if HTML assets were copied, `false` otherwise.

## Description
When suggestions aren't available for a keyboard, an HTML banner is displayed instead.
Use this method to specify any HTML assets the banner will use to theme your keyboard app. Some examples of assets would be .svg images or .css files used in your banner.

This can be called towards the end of `SystemKeyboard.onCreate()`.

You still need to call [setHTMLBanner()](setHTMLBanner) for Keyman Engine to assign the HTML banner for in-app and system keyboards.

## Examples

### Example: Using `copyHTMLBannerAssets()`

The following script illustrates the use of `copyHTMLBannerAssets()`:
```javascript
    // Copies HTML banner assets located in /assets/banner/
    KMManager.copyHTMLBannerAssets(this, "banner");
```

## See also
* [setHTMLBanner()](setHTMLBanner)
