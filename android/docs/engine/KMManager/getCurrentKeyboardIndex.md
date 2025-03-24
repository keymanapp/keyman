---
title: KMManager.getCurrentKeyboardIndex()
---

## Summary

The **`getCurrentKeyboardIndex()`** method returns index number of the
current keyboard in keyboards list.

## Syntax

``` javascript
KMManager.getCurrentKeyboardIndex(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns 0-based index number of the current keyboard if exists in
keyboards list, -1 otherwise.

## Description

Use this method to get the index number of the currently selected
keyboard if it exists in keyboards list.

## Examples

### Example: Using `getCurrentKeyboardIndex()`

The following script illustrate the use of `getCurrentKeyboardIndex()`:

``` javascript
    int index = KMManager.getCurrentKeyboardIndex(this);
```

## See also

-   [`getCurrentKeyboardInfo()`](getCurrentKeyboardInfo)
-   [`getKeyboardIndex()`](getKeyboardIndex)
-   [`getKeyboardInfo()`](getKeyboardInfo)
-   [`getKeyboardsList()`](getKeyboardsList)
-   [`keyboardExists()`](keyboardExists)
