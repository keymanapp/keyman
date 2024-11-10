---
title: KMManager.getKeyboardIndex()
---

## Summary

The **`getKeyboardIndex()`** method returns index number of the
specified keyboard in keyboards list.

## Syntax

``` javascript
KMManager.getKeyboardIndex(Context context, String keyboardID, String languageID)
```

### Parameters

`context`
:   The context.

`keyboardID`
:   ID of the keyboard.

`languageID`
:   ID of the associated language.

### Returns

Returns 0-based index number of the specified keyboard if exists in
keyboards list, -1 otherwise.

## Description

Use this method to get the index number of the keyboard with given
keyboard ID and language ID if it exists in keyboards list.

## Examples

### Example: Using `getKeyboardIndex()`

The following script illustrate the use of `getKeyboardIndex()`:

``` javascript
    int index = KMManager.getKeyboardIndex(this, "tamil99m", "ta");
```

## See also

-   [`getCurrentKeyboardIndex()`](getCurrentKeyboardIndex)
-   [`getCurrentKeyboardInfo()`](getCurrentKeyboardInfo)
-   [`getKeyboardInfo()`](getKeyboardInfo)
-   [`getKeyboardsList()`](getKeyboardsList)
-   [`keyboardExists()`](keyboardExists)
