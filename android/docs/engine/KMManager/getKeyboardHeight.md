---
title: KMManager.getKeyboardHeight()
---

## Summary

The **`getKeyboardHeight()`** method returns the height of the keyboard
frame.

## Syntax

``` javascript
KMManager.getKeyboardHeight(Context context)
```
or
``` javascript
KMManager.getKeyboardHeight(Context context, int orientation)
```

### Parameters

`context`
:   The context.

`orientation` _(Optional)_
: Accepts a [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) value. This is most useful if you want to get the size of the keyboard in the other (non-current) orientation. If `orientation` parameter is not passed in, keyboard height is returned for the current device orientation.

### Returns

Returns the height of the keyboard frame in *density-independent pixels
(dp)*.

## Description

Use this method to get the height of the keyboard frame.

## Examples

### Example: Using `getKeyboardHeight()`

The following script illustrate the use of `getKeyboardHeight()`:

``` javascript
    int keyboardHeight = KMManager.getKeyboardHeight(this);
```
or
```java
    import android.content.res.Configuration;
    ...
    // Get the current Keyman keyboard height for landscape mode.
    
    int keyboardHeightLandscape = KMManager.getKeyboardHeight(this, Configuration.ORIENTATION_LANDSCAPE);
```

## See also

-   [applyKeyboardHeight()](applyKeyboardHeight)
-   [getKeyboardHeightMax](getKeyboardHeightMax)
-   [getKeyboardHeightMin](getKeyboardHeightMin)
