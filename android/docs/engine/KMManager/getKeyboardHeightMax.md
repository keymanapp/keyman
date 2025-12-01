---
title: KMManager.getKeyboardHeightMax()
---

## Summary

The **`getKeyboardHeightMax()`** method returns the maximum allowed height of the keyboard frame for this context.

## Syntax

``` javascript
KMManager.getKeyboardHeightMax(Context context)
```
or
``` javascript
KMManager.getKeyboardHeightMax(Context context, int orientation)
```

### Parameters

`context`
:   The context.

`orientation` _(Optional)_
: Accepts a [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) value. This is most useful if you want to get the size of the keyboard in the other (non-current) orientation. If `orientation` is not defined, maximum keyboard height is returned for the current device orientation.

### Returns

Returns the maximum allowed height of the keyboard frame in *density-independent pixels
(dp)*.

## Description

Use this method to get the maximum allowed height of the keyboard frame.

## Examples

### Example: Using `getKeyboardHeightMax()`

The following script illustrate the use of `getKeyboardHeightMax()`:

``` javascript
    int maxKeyboardHeight = KMManager.getKeyboardHeightMax(this);
```
or
```java
    import android.content.res.Configuration;
    ...
    // Get the maximum allowed Keyman keyboard height for landscape mode.
    int maxKeyboardHeightLandscape = KMManager.getKeyboardHeightMax(this, Configuration.ORIENTATION_LANDSCAPE);
```

## See also

-   [applyKeyboardHeight()](applyKeyboardHeight)
-   [getKeyboardHeight()](getKeyboardHeight)
-   [getKeyboardHeightMin()](getKeyboardHeightMin)
