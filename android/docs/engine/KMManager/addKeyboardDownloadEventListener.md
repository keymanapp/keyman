---
title: KMManager.addKeyboardDownloadEventListener()
---

## Summary

The **`addKeyboardDownloadEventListener()`** method adds the specified
listener into the list of keyboard download event listeners.

## Syntax

``` javascript
KMManager.addKeyboardDownloadEventListener(OnKeyboardDownloadEventListener listener)
```

### Parameters

`listener`
:   The listener to receive keyboard download event notifications.

## Description

Use this method to add a listener to receive keyboard download event
notifications. The listener must implement
`KMManager.OnKeyboardDownloadEventListener` interface.

## Examples

### Example: Using `addKeyboardDownloadEventListener()`

The following script illustrate the use of
`addKeyboardDownloadEventListener()`:

``` javascript
    @Override
    protected void onResume() {
        super.onResume();
        // ...
        KMManager.addKeyboardDownloadEventListener(this);
        // ...
    }
```

## See also

-   [`removeKeyboardDownloadEventListener()`](removeKeyboardDownloadEventListener)
