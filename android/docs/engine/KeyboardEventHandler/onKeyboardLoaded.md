---
title: onKeyboardLoaded()
---

## Summary

The **`onKeyboardLoaded()`** event is called when the keyboard has been
loaded for the first time.

## Syntax

``` javascript
public void onKeyboardLoaded(KeyboardType keyboardType)
```

### Parameters

`keyboardType`
:   The keyboard type that has been loaded. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

## Description

Implement this method to handle keyboard loaded event.

## Examples

### Example: Using `onKeyboardLoaded()`

The following script illustrate the use of `onKeyboardLoaded()`:

``` javascript
    @Override
    public void onKeyboardLoaded(KeyboardType keyboardType) {
        // handle keyboard loaded event here
    }
```

## See also

-   [`onKeyboardChanged()`](onKeyboardChanged)
-   [`onKeyboardDismissed()`](onKeyboardDismissed)
-   [`onKeyboardShown()`](onKeyboardShown)
