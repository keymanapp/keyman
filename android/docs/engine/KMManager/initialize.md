---
title: KMManager.initialize()
---

## Summary

The **`initialize()`** method initializes the Keyman manager.

## Syntax

``` javascript
KMManager.initialize(Context context, KeyboardType keyboardType)
```

### Parameters

`context`
:   The context that starts the initialization. This is normally an
    Activity or the application context of an InputMethodService.

`keyboardType`
:   KeyboardType to be used. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

## Description

This method is normally called from `onCreate()` method of an Activity
or InputMethodService.

## Examples

### Example: Using `initialize()`

The following script illustrate the use of `initialize()`:

``` javascript
@Override
protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    // ...
    KMManager.initialize(this, KeyboardType.KEYBOARD_TYPE_INAPP);
    // ...
    // do not call setContentView before initializing the Keyman manager.
    setContentView(R.layout.activity_main);
    // ...
}
```
