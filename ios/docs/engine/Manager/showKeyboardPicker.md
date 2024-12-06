---
title: Manager.showKeyboardPicker()
---

## Summary

The **`showKeyboardPicker()`** method displays a list of available
keyboards and allows a user to add/download new keyboards or remove
existing ones.

## Syntax

``` swift
Manager.showKeyboardPicker(viewController: UIViewController, shouldAddKeyboard: Bool)
```

### Parameters

`viewController`
:   The current UIViewController (recommended) or the navigation controller.

`shouldAddKeyboard`
:   Whether to immediately open the subview to add a new keyboard. If set to `false`, the language picker won't be displayed

## Description

Use this method to display keyboard picker menu. Normally you do not
need to call this method explicitly since, by default, Keyman on-screen
keyboard calls this method to display the keyboard picker menu whenever
'globe' key is tapped. Multiple calls to this method are unsafe and may
result in multiple instances of keyboard picker menu being displayed at
the same time.

## Examples

### Example: Using `showKeyboardPicker()`

The following script illustrates the use of `showKeyboardPicker()`:

``` swift
mainViewController.dismissGetStartedView(nil)
Manager.shared.showKeyboardPicker(in: mainViewController, shouldAddKeyboard: true)
```
