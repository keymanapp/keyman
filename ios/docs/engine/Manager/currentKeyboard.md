---
title: Manager.currentKeyboard
---

## Summary

The **`currentKeyboard`** method returns info for the current keyboard, if its set.

## Syntax

``` swift
Manager.currentKeyboard
```

### Returns

Returns the keyboard information `InstallableKeyboard`.

## Description

Use this method to get the current keyboard information.

## Examples

### Using `currentKeyboard`

The following script illustrates the use of `currentKeyboard`:

``` swift
    let keyboardInfo = Manager.shared.currentKeyboard
    let currentKeyboardId = keyboardInfo?.id ?? Defaults.keyboard.id
```
