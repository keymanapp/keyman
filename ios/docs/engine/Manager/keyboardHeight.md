---
title: Manager.KeyboardHeight
---

## Summary

The **`keyboardHeight`** method returns the keyboard height, based on whether the keyboard is a system keyboard and the device orientation.

## Syntax

``` swift
Manager.keyboardHeight
```

### Returns

Returns the keyboard height (CGFloat)

## Description

This syntax can be used for getting the keyboard height, including any space occupied by its banner if shown.

## Examples

### Example 1: Using `keyboardHeight`

The following script illustrates the use of `keyboardHeight`:

``` swift
let kbWidth = keyboardWidth
let kbHeight = keyboardHeight
keymanWeb.frame = CGRect(x: 0.0, y: 0.0, width: kbWidth, height: kbHeight + 1000)
```

------------------------------------------------------------------------

## See also

-   [`addKeyboard()`](addKeyboard)
-   [`switchToNextKeyboard()`](switchToNextKeyboard)
