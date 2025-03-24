---
title: onKeyboardDownloadFinished()
---

## Summary

The **`onKeyboardDownloadFinished()`** event is called when a keyboard
download has finished.

## Syntax

``` javascript
public void onKeyboardDownloadFinished(HashMap<String key, String value> keyboardInfo, int result)
```

### Parameters

`keyboardInfo`
:   The information dictionary of the keyboard with keys and values
    defined as `HashMap<String key, String value>`.

`result`
:   The result of the download (result &gt; 0 if successful, &lt; 0 if
    failed).

## Description

Implement this method to handle keyboard download finished event.

## Examples

### Example: Using `onKeyboardDownloadFinished()`

The following script illustrate the use of
`onKeyboardDownloadFinished()`:

``` javascript
    @Override
    public void onKeyboardDownloadFinished(HashMap<String key, String value> keyboardInfo, int result) {
        // handle keyboard download finished event here
    }
```

## See also

-   [`onKeyboardDownloadStarted()`](onKeyboardDownloadStarted)
