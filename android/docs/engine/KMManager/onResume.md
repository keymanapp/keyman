---
title: KMManager.onResume()
---

## Summary

The **`onResume()`** method performs necessary actions in an Activity's
`onResume()`.

## Syntax

``` javascript
KMManager.onResume()
```

## Description

To be called from an Activity's `onResume()` method.

## Examples

### Example: Using `onResume()`

The following script illustrate the use of `onResume()`:

``` javascript
@Override
protected void onResume() {
    super.onResume();
    KMManager.onResume();
    // ...
}
```

## See also

-   [`onPause()`](onPause)
