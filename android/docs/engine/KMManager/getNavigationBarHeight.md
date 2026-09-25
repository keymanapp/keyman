---
title: KMManager.getNavigationBarHeight()
---

## Summary

The **`getNavigationBarHeight()`** method returns the height of the navigation bar corresponding to the in-app or system keyboard.

## Syntax

``` javascript
KMManager.getNavigationBarHeight(Context context, KeyboardType keyboardType)
```

### Parameters

`context`
:   The context.

`keyboardType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.)

### Returns

Returns the height of the navigation bar in pixels.

## Description

Use this method to get the height of the navigation bar, which depends on 
the keyboard type (in-app or system) and navigation mode (gesture, 2-button, or 3-button navigation).
The keyboard offsets must account for this height below the keyboard.

## Examples

### Example: Using `getNavigationBarHeight()`

The following script illustrate the use of `getNavigationBarHeight()`:

``` javascript
    int navigationHeight = KMManager.getNavigationBarHeight(this, KeyboardType.SYSTEM_KEYBOARD);

    outInsets.contentTopInsets = inputViewHeight - bannerHeight - kbHeight - navigationHeight;
```

## See Also

-   [`getBannerHeight()`](getBannerHeight)
-   [`getKeyboardHeight()`](getKeyboardHeight)
