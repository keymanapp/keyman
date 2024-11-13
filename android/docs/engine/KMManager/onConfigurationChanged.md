---
title: KMManager.onConfigurationChanged()
---

## Summary

The **`onConfigurationChanged()`** method performs necessary actions in
an InputMethodService's `onConfigurationChanged()`.

## Syntax

``` javascript
KMManager.onConfigurationChanged(Configuration newConfig)
```

### Parameters

`newConfig`
:   The new device configuration.

## Description

To be called from an InputMethodService's `onConfigurationChanged()`
method.

## Examples

### Example: Using `onConfigurationChanged()`

The following script illustrate the use of `onConfigurationChanged()`:

``` javascript
@Override
public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);
    KMManager.onConfigurationChanged(newConfig);
    // ...
}
```

## See also

-   [`createInputView()`](createInputView)
-   [`onStartInput()`](onStartInput)
-   [`onDestroy()`](onDestroy)
