---
title: KMManager.canAddNewKeyboard()
---

## Summary

The **`canAddNewKeyboard()`** method returns whether adding a new
keyboard is enabled.

## Syntax

``` javascript
KMManager.canAddNewKeyboard()
```

### Returns

Returns `true` if adding a new keyboard is enabled, `false` otherwise.

## Description

Use this method to check if additional keyboards can be added.

## Examples

### Example: Using `canAddNewKeyboard()`

The following script illustrate the use of `canAddNewKeyboard()`:

``` javascript
    Keyboard kbInfo = ...; // Keyboard information
    if (KMManager.canAddNewKeyboard()) {
        KMManager.addKeyboard(this, kbInfo);
    }
```

## See also

-   [`canRemoveKeyboard`](canRemoveKeyboard)
-   [`setCanAddNewKeyboard`](setCanAddNewKeyboard)
