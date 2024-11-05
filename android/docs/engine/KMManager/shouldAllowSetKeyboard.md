---
title: KMManager.shouldAllowSetKeyboard()
---

## Summary

The **`shouldAllowSetKeyboard()`** method returns whether Keyman Engine
allows setting a keyboard other than the default keyboard.

## Syntax

``` javascript
KMManager.shouldAllowSetKeyboard()
```

### Returns

Returns `true` if Keyman Engine allows setting a keyboard, `false`
otherwise.

## Description

Use this method to check if Keyman Engine allows setting a keyboard
other than the default keyboard.

## Examples

### Example: Using `shouldAllowSetKeyboard()`

The following script illustrate the use of `shouldAllowSetKeyboard()`:

``` javascript
    if (KMManager.shouldAllowSetKeyboard()) {
        // setting a keyboard is allowed
    }
    else {
        // setting a keyboard is not allowed
    }
```

## See also

-   [`setShouldAllowSetKeyboard`](setShouldAllowSetKeyboard)
