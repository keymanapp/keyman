---
title: onKeyboardDownloadStarted()
---

## Summary

The **`onKeyboardDownloadStarted()`** event is called when a keyboard
download has started.

## Syntax

``` javascript
public void onKeyboardDownloadStarted(HashMap<String key, String value> keyboardInfo)
```

### Parameters

`keyboardInfo`
:   The information dictionary of the keyboard with keys and values
    defined as `HashMap<String key, String value>`.

## Description

Implement this method to handle keyboard download started event.

## Examples

### Example: Using `onKeyboardDownloadStarted()`

The following script illustrate the use of
`onKeyboardDownloadStarted()`:

``` javascript
    @Override
    public void onKeyboardDownloadStarted(HashMap<String key, String value> keyboardInfo) {
        // handle keyboard download started event here
    }
```

## See also

-   [`onKeyboardDownloadFinished()`](onKeyboardDownloadFinished)
