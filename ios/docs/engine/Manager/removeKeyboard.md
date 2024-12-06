---
title: Manager.removeKeyboard()
---

## Summary

The **`removeKeyboard()`** method removes a keyboard from the keyboard
list by a specified keyboard ID or position.

## Syntax

``` swift
Manager.removeKeyboard(fullID: FullKeyboardID)
```

### Parameter

`fullID`
:   The full keyboard ID.

### Returns

Returns `true` if the keyboard exists and was removed, `false`
otherwise.

## Description

Use this method to remove a keyboard from the list in the keyboard
picker. If the keyboard doesn't exist, it will be ignored and the method
will return `false` without altering the keyboards list.

## Examples

### Example: Using `removeKeyboard()`

The following script illustrates the use of `removeKeyboard()`:

``` swift
Manager.removeKeyboard('tamil99');
```

  

------------------------------------------------------------------------

## Syntax

``` swift
Manager.removeKeyboard(index: Int)
```

### Parameter

`index`
:   Index of keyboard to remove from the keyboards list.

### Returns

Returns `true` if the keyboard exists and was removed, `false`
otherwise.

## Description

Use this method to remove the keyboard at index from the keyboards list.
If the keyboard doesn't exist, it will be ignored and the method will
return `false` without altering the keyboards list.

## Examples

### Example: Using `removeKeyboard()`

The following script illustrates the use of `removeKeyboard()`:

``` swift
if Manager.shared.removeKeyboard(at: indexPath.row) {
  loadUserKeyboards()
}
```

## See also

-   [`addKeyboard()`](addKeyboard)
