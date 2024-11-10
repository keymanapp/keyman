---
title: KMManager.setCanAddNewKeyboard()
---

## Summary

The **`setCanAddNewKeyboard()`** method sets whether adding a new
keyboard is allowed.

## Syntax

``` javascript
KMManager.setCanAddNewKeyboard(boolean newValue)
```

### Parameters

`newValue`
:   If `false`, adding a new keyboard is disabled.

## Description

Use this method to enable or disable '+' (add new keyboard) button in
the keyboard picker menu.

## Examples

### Example: Using `setCanAddNewKeyboard()`

The following script illustrate the use of `setCanAddNewKeyboard()`:

``` javascript
    // Disable add new keyboard button.
    KMManager.setCanAddNewKeyboard(false);
```

## See also

-   [`canAddNewKeyboard`](canAddNewKeyboard)
