---
title: KeyboardPickerBarButtonItem class
---

## Summary

Returns a UIBarButtonItem that will display the keyboard picker when tapped.

## Syntax

``` swift
KeyboardPickerBarButtonItem(presentingVC: UIViewController)
```

### Parameters

`presentingVC`
:   View controller to handle the keyboard picker modes.

## Description

Opens a view for the user to switch keyboards or download new keyboards.

Since the keyboard picker is modal, a UIViewController must be supplied to display it. The button has default images for normal and landscape orientation, and can overridden with other images or a title.

## Examples

### Example: Using `KeyboardPickerBarButtonItem()`

The following script illustrates the use of `KeyboardPickerBarButtonItem()`:

``` swift
let kbBarButton = KeyboardPickerBarButtonItem(presentingVC: self)
```

## See also

-   [`KeyboardPickerButton()`](../KeyboardPickerButton/)
