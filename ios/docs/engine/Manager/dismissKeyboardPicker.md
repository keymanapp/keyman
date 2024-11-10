---
title: Manager.dismissKeyboardPicker()
---

## Summary

The **`dismissKeyboardPicker()`** method hides the keyboard picker.

## Syntax

``` swift
Manager.dismissKeyboardPicker(viewController: UIViewController)
```

### Parameters

`viewController`
:   View controller.

## Description

This dismisses the language list.

## Examples

### Using `dismissKeyboardPicker`

The following script illustrates the use of `dismissKeyboardPicker`:

``` swift
if !_isDoneButtonEnabled {
  Manager.shared.dismissKeyboardPicker(self)
}
```

## See also

-   [`showKeyboardPicker()`](showKeyboardPicker)
