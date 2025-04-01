---
title: KMManager.setBannerOptions()
---

## Summary

The **`setBannerOptions()`** method sets options relating to the predictive text banner, such as controlling whether to enable predictions.

## Syntax

``` javascript
KMManager.setBannerOptions(boolean mayPredict, KeyboardType keyboardType)
```

### Parameters

`mayPredict`
:   If `false`, KeymanWeb engine will disable predications and not show suggestions on the banner.

`keyboardType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

## Description

Use this method to enable or disable the KeymanWeb engine from generating and displaying predictions on the suggestion banner.

## Examples

### Example: Using `setBannerOptions()`

The following script illustrate the use of `setBannerOptions()`:

``` javascript

    // Temporarily disable predictions on certain fields (e.g. hidden password field or numeric)
    inputType = attribute.inputType;
    KMManager.setPredictionsSuspended(inputType, KeyboardType.KEYBOARD_TYPE_SYSTEM);
    if (KMManager.getPredictionsSuspended(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
      KMManager.setBannerOptions(false, KeyboardType.KEYBOARD_TYPE_SYSTEM);
      // Set the system keyboard HTML banner
      BannerController.setHTMLBanner(this, KeyboardType.KEYBOARD_TYPE_SYSTEM);
    }
```

## History
Added in Keyman Engine for Android 18.0.

## See also

-   [`getPredictionsSuspended`](getPredictionsSuspended)
-   [`setPredictionsSuspended`](setPredictionsSuspended)
