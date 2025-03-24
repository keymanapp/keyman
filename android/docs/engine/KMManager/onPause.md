---
title: KMManager.onPause()
---

## Summary

The **`onPause()`** method performs necessary actions in an Activity's
`onPause()`.

## Syntax

``` javascript
KMManager.onPause()
```

## Description

To be called from an Activity's `onPause()` method.

## Examples

### Example: Using `onPause()`

The following script illustrate the use of `onPause()`:

``` javascript
@Override
protected void onPause() {
    super.onPause();
    KMManager.onPause();
    // ...
}
```

## See also

-   [`onResume()`](onResume)
