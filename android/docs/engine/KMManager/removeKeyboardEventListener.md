---
title: KMManager.removeKeyboardEventListener()
---

## Summary

The **`removeKeyboardEventListener()`** method removes the specified
listener from the list of keyboard event listeners.

## Syntax

``` javascript
KMManager.removeKeyboardEventListener(OnKeyboardEventListener listener)
```

### Parameters

`listener`
:   The listener to be removed.

## Description

Use this method to remove the listener to stop receiving keyboard event
notifications. The listener must be removed once you finished with it.

## Examples

### Example: Using `removeKeyboardEventListener()`

The following script illustrate the use of
`removeKeyboardEventListener()`:

``` javascript
    @Override
    protected void onPause() {
        super.onPause();
        // ...
        KMManager.removeKeyboardEventListener(this);
        // ...
    }
```

## See also

-   [`addKeyboardEventListener()`](addKeyboardEventListener)
