---
title: KMManager.getKeyboardHeightMin()
---

## Summary

The **`getKeyboardHeightMin()`** method returns the minimum allowed height of the keyboard frame for this context.

## Syntax

``` javascript
KMManager.getKeyboardHeightMin(Context context)
```
or
``` javascript
KMManager.getKeyboardHeightMin(Context context, int orientation)
```

### Parameters

`context`
:   The context.

`orientation` _(Optional)_
: Accepts a [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) value. This is most useful if you want to get the size of the keyboard in the other (non-current) orientation. If `orientation` is not defined, minimum keyboard height is returned for the current device orientation.

### Returns

Returns the minimum allowed height of the keyboard frame in *density-independent pixels
(dp)*.

## Description

Use this method to get the minimum allowed height of the keyboard frame.

## Examples

### Example: Using `getKeyboardHeightMin()`

The following script illustrate the use of `getKeyboardHeightMin()`:

``` javascript
    int minKeyboardHeight = KMManager.getKeyboardHeightMin(this);
```
or
```java
    import android.content.res.Configuration;
    ...
    // Get the minimum allowed Keyman keyboard height for landscape mode.
    int minKeyboardHeightLandscape = KMManager.getKeyboardHeightMin(this, Configuration.ORIENTATION_LANDSCAPE);
```

## See also

-   [applyKeyboardHeight()](applyKeyboardHeight)
-   [getKeyboardHeight()](getKeyboardHeight)
-   [getKeyboardHeightMax()](getKeyboardHeightMax)
