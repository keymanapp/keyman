---
title: KMManager.removeKeyboardDownloadEventListener()
---

## Summary

The **`removeKeyboardDownloadEventListener()`** method removes the
specified listener from the list of keyboard download event listeners.

## Syntax

``` javascript
KMManager.removeKeyboardDownloadEventListener(OnKeyboardDownloadEventListener listener)
```

### Parameters

`listener`
:   The listener to be removed.

## Description

Use this method to remove the listener to stop receiving keyboard
download event notifications. The listener must be removed once you
finished with it.

## Examples

### Example: Using `removeKeyboardDownloadEventListener()`

The following script illustrate the use of
`removeKeyboardDownloadEventListener()`:

``` javascript
    @Override
    protected void onPause() {
        super.onPause();
        // ...
        KMManager.removeKeyboardDownloadEventListener(this);
        // ...
    }
```

## See also

-   [`addKeyboardDownloadEventListener()`](addKeyboardDownloadEventListener)
