---
title: KeyboardPickerButton.setTitle()
---

## Summary

The **`setTitle()`** method sets the title of the keyboard picker button.

## Syntax

``` swift
KeyboardPickerButton.setTitle(title: String, state: UIControlState)
```

### Parameters

`title`
:   Title for the button.

`state`
:   State of the button

## Description

Clear images if the developer sets a title.

## Examples

### Example: Using `setTitle()`

The following script illustrates the use of `setTitle()`:

``` swift
let kbButton = KeyboardPickerButton(presentingVC: self)
kbButton.setTitle(item.title, for: .normal)
```
