---
title: KMManager.shouldCheckKeyboardUpdates()
---

## Summary

The **`shouldCheckKeyboardUpdates()`** method returns whether Keyman
Engine should check for keyboard updates.

## Syntax

``` javascript
KMManager.shouldCheckKeyboardUpdates()
```

### Returns

Returns `true` if Keyman Engine should check for keyboard updates,
`false` otherwise.

## Description

Use this method to check if Keyman Engine should check for keyboard
updates when the keyboard picker menu is displayed.

## Examples

### Example: Using `shouldCheckKeyboardUpdates()`

The following script illustrate the use of
`shouldCheckKeyboardUpdates()`:

``` javascript
    if (KMManager.shouldCheckKeyboardUpdates()) {
        // checking keyboard updates is enabled
    }
```

## See also

-   [`setShouldCheckKeyboardUpdates`](setShouldCheckKeyboardUpdates)
