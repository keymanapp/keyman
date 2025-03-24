---
title: KMManager.hideSystemKeyboard()
---

## Summary

The **`hideSystemKeyboard()`** method hides the system OSK.

## Syntax

``` javascript
KMManager.hideSystemKeyboard()
```

## Description

Use this method to hide the system OSK. A common usage is to prevent
your app from displaying both in-app OSK and the system OSK.

## Examples

### Example: Using `hideSystemKeyboard()`

The following script illustrate the use of `hideSystemKeyboard()`:

``` javascript
protected void onResume() {
  super.onResume();
  KMManager.onResume();
  KMManager.hideSystemKeyboard();
  // ...
```

## See also

-   [`onResume()`](onResume)
