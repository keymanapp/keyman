---
title: KMManager.addKeyboardEventListener()
---

## Summary

The **`addKeyboardEventListener()`** method adds the specified listener
into the list of keyboard event listeners.

## Syntax

``` javascript
KMManager.addKeyboardEventListener(OnKeyboardEventListener listener)
```

### Parameters

`listener`
:   The listener to receive keyboard event notifications.

## Description

Use this method to add a listener to receive keyboard event
notifications. The listener must implement
KMManager.OnKeyboardEventListener interface.

## Examples

### Example: Using `addKeyboardEventListener()`

The following script illustrate the use of `addKeyboardEventListener()`:

``` javascript
    @Override
    protected void onResume() {
        super.onResume();
        // ...
        KMManager.addKeyboardEventListener(this);
        // ...
    }
```

## See also

-   [`removeKeyboardEventListener()`](removeKeyboardEventListener)
