---
title: onKeyboardChanged()
---

## Summary

The **`onKeyboardChanged()`** event is called when another keyboard has
been set.

## Syntax

``` javascript
public void onKeyboardChanged(String newKeyboard)
```

### Parameters

`newKeyboard`
:   New keyboard identifier as languageID_keyboardID (e.g. eng_us).

## Description

Implement this method to handle keyboard changed event.

## Examples

### Example: Using `onKeyboardChanged()`

The following script illustrate the use of `onKeyboardChanged()`:

``` javascript
    @Override
    public void onKeyboardChanged(String newKeyboard) {
        // handle keyboard changed event here
    }
```

## See also

-   [`onKeyboardDismissed()`](onKeyboardDismissed)
-   [`onKeyboardLoaded()`](onKeyboardLoaded)
-   [`onKeyboardShown()`](onKeyboardShown)
