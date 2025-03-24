---
title: KMManager.setShouldCheckKeyboardUpdates()
---

## Summary

The **`setShouldCheckKeyboardUpdates()`** method sets whether Keyman
Engine should check for keyboard updates.

## Syntax

``` javascript
KMManager.setShouldCheckKeyboardUpdates(boolean newValue)
```

### Parameters

`newValue`
:   If `false`, Keyman Engine will not check for keyboard updates.

## Description

Use this method to enable or disable keyboard updates when the keyboard
picker menu is displayed.

## Examples

### Example: Using `setShouldCheckKeyboardUpdates()`

The following script illustrate the use of
`setShouldCheckKeyboardUpdates()`:

``` javascript
    // Disable keyboard updates.
    KMManager.setShouldCheckKeyboardUpdates(false);
```

## See also

-   [`shouldCheckKeyboardUpdates`](shouldCheckKeyboardUpdates)
