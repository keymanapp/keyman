---
title: Manager.switchToNextKeyboard()
---

## Summary

The **`switchToNextKeyboard()`** method switches to the next keyboard.

## Syntax

``` swift
Manager.switchToNextKeyboard()
```

### Returns

Returns the index of the newly selected keyboard. If there's no keyboard to switch to, returns `nil`.

## Description

Use this method to switch to next keyboard in the keyboards list. If the
next keyboard does not exists, then it loads the first keyboard in the
keyboards list.

## Examples

### Example: Using `switchToNextKeyboard()`

The following script illustrates the use of `switchToNextKeyboard()`:

``` swift
Manager.shared.switchToNextKeyboard()
```

## See also

-   [`setKeyboard()`](setKeyboard)
