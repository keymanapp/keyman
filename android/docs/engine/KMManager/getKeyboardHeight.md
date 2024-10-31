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

### Parameters

`context`
:   The context.

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

## See also

-   [applyKeyboardHeight](applyKeyboardHeight)
