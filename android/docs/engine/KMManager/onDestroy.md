---
title: KMManager.onDestroy()
---

## Summary

The **`onDestroy()`** method performs necessary actions in an
InputMethodService's `onDestroy()`.

## Syntax

``` javascript
KMManager.onDestroy()
```

## Description

To be called from an InputMethodService's `onDestroy()` method.

## Examples

### Example: Using `onDestroy()`

The following script illustrate the use of `onDestroy()`:

``` javascript
@Override
public void onDestroy() {
    // ...
    KMManager.onDestroy();
    super.onDestroy();
}
```

## See also

-   [`createInputView()`](createInputView)
-   [`onStartInput()`](onStartInput)
-   [`onConfigurationChanged()`](onConfigurationChanged)
