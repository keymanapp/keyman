---
title: KMManager.getDefaultKeyboardHeight()
---

## Summary

The **`getDefaultKeyboardHeight()`** method returns the default height of the keyboard frame for the current or specefied orientation.

## Syntax

``` javascript
KMManager.getDefaultKeyboardHeight(Context context)
```
or
``` javascript
KMManager.getDefaultKeyboardHeight(int orientation)
```

### Parameters (Choose One)

`context` _(Alternate)_
:   The context.

`orientation` _(Alternate)_
: Accepts a [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) value. This is most useful if you want to get the size of the keyboard in the other (non-current) orientation. If `orientation` parameter is not passed in, keyboard height is returned for the current device orientation.

### Returns

Returns the default height of the keyboard frame in *density-independent pixels (dp)*.

## Description

Use this method to get the default height of the keyboard frame.

## Examples

### Example: Using `getDefaultKeyboardHeight()`

The following script illustrate the use of `getDefaultKeyboardHeight()`:

``` javascript
    int keyboardHeight = KMManager.getDefaultKeyboardHeight(this);
```
or
```java
    import android.content.res.Configuration;
    ...
    // Get the current Keyman keyboard height for landscape mode.
    
    int keyboardHeightLandscape = KMManager.getDefaultKeyboardHeight( Configuration.ORIENTATION_LANDSCAPE);
```

## See also

-   [applyKeyboardHeight()](applyKeyboardHeight)
