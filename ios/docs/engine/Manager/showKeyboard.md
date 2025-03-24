---
title: Manager.showKeyboard()
---

## Summary

The **`showKeyboard()`** method shows the in-app OSK.

## Syntax

``` swift
Manager.showKeyboard()
```

## Description

Use this method to restore the in-app OSK after it has been hidden with
`hideKeyboard`. One common case is to restore the OSK after dismissal of
an app's popup notifications or windows.

## Examples

### Example: Using `showKeyboard()`

The following script illustrates the use of `showKeyboard()` in the
Keyman for iPhone and iPad app's "Getting Started" view so that the
keyboard doesn't hide the lower options on certain devices

``` swift
Manager.shared.showKeyboard()
```

## See also

-   [`hideKeyboard()`](hideKeyboard)
