---
title: KMManager.removeKeyboard()
---

## Summary

The **`removeKeyboard()`** method removes the keyboard at specified
position from the keyboards list.

## Syntax

``` javascript
KMManager.removeKeyboard(Context context, int position)
```

### Parameters

`context`
:   The context.

`position`
:   0-based position of the keyboard in the keyboards list.

### Returns

Returns `true` if the keyboard was removed successfully, `false`
otherwise.

## Description

Use this method to remove a keyboard from the keyboards list. If the
position is invalid, it will be ignored and the method will return
`false` without altering the keyboards list.

## Examples

### Example: Using `removeKeyboard()`

The following script illustrate the use of `removeKeyboard()`:

``` javascript
    // Remove the second keyboard in the list
    KMManager.removeKeyboard(this, 1);
```

## See also

-   [`addKeyboard()`](addKeyboard)
