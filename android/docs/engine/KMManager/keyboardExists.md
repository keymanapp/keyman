---
title: KMManager.keyboardExists()
---

## Summary

The **`keyboardExists()`** method returns whether the specified keyboard
exists in keyboards list.

## Syntax

``` javascript
KMManager.keyboardExists(Context context, String keyboardID, String languageID)
```

### Parameters

`context`
:   The context.

`keyboardID`
:   ID of the keyboard.

`languageID`
:   ID of the associated language.

### Returns

Returns `true` if the keyboard exists in keyboards list, `false`
otherwise.

## Description

Use this method to check if the keyboard with given keyboard ID and
language ID exists in keyboards list.

## Examples

### Example: Using `keyboardExists()`

The following script illustrate the use of `keyboardExists()`:

``` javascript
    boolean keyboardExists = KMManager.keyboardExists(this, "tamil99m", "ta");
```

## See also

-   [`getCurrentKeyboardIndex()`](getCurrentKeyboardIndex)
-   [`getCurrentKeyboardInfo()`](getCurrentKeyboardInfo)
-   [`getKeyboardIndex()`](getKeyboardIndex)
-   [`getKeyboardInfo()`](getKeyboardInfo)
-   [`getKeyboardsList()`](getKeyboardsList)
