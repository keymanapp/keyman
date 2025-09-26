---
title: KMManager.getNavigationBarHeight()
---

## Summary

The **`getNavigationBarHeight()`** method returns the height of the navigation bar.

## Syntax

``` javascript
KMManager.getNavigationBarHeight(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns the height of the navigation bar in pixels.

## Description

Use this method to get the height of the navigation bar. The keyboard offsets must account for this height below the keyboard.

## Examples

### Example: Using `getNavigationBarHeight()`

The following script illustrate the use of `getNavigationBarHeight()`:

``` javascript
    int navigationHeight = KMManager.getNavigationBarHeight(this);

```

## See Also

-   [`getBannerHeight()`](getBannerHeight)
-   [`getKeyboardHeight()`](getKeyboardHeight)
