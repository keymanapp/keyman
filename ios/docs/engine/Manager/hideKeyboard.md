---
title: Manager.hideKeyboard()
---

## Summary

The **`hideKeyboard()`** method hides the in-app OSK.

## Syntax

``` swift
Manager.hideKeyboard()
```

## Description

A common usage is to hide the in-app OSK so that it does not obscure part of an app's dismissible popup notifications or windows.

## Examples

### Using `hideKeyboard`

The following script illustrates the use of `hideKeyboard`:

``` swift
Manager.shared.hideKeyboard()
```

## See also

-   [`showKeyboard()`](showKeyboard)
