---
title: KMManager.setShouldAllowSetKeyboard()
---

## Summary

The **`setShouldAllowSetKeyboard()`** method sets whether Keyman Engine
allows setting a keyboard other than the default keyboard.

## Syntax

``` javascript
KMManager.setShouldAllowSetKeyboard(boolean value)
```

### Parameters

`value`
:   If `false`, Keyman Engine will not allow setting a keyboard.

## Description

Use this method to enable or disable setting a keyboard other than the
default keyboard. If set to `false` Keyman Engine will immediately load
the default keyboard, and ignore calls to setKeyboard method. It is
particularly useful if used with Google Play Licensing service in order
to put the Keyman on-screen keyboard in a locked state if the paid app
is unlicensed.

## Examples

### Example: Using `setShouldAllowSetKeyboard()`

The following script illustrate the use of
`setShouldAllowSetKeyboard()`:

``` javascript
    // Put Keyman on-screen keyboard in locked state if the app is unlicensed.
    KMManager.setShouldAllowSetKeyboard(false);
```

## See also

-   [`shouldAllowSetKeyboard`](shouldAllowSetKeyboard)
