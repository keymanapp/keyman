---
title: KMManager.getLatestKeyboardFileVersion()
---

## Summary

The **`getLatestKeyboardFileVersion()`** method returns the specified
keyboard's latest file version number.

## Syntax

``` javascript
KMManager.getLatestKeyboardFileVersion(Context context, String packageID, String keyboardID)
```

### Parameters

`context`
:   The context.

`packageID`
:   ID of the package.

`keyboardID`
:   ID of the keyboard.

### Returns

Returns the specified keyboard's latest file version number as `String`
if the keyboard exists, `null` otherwise.

## Description

Use this method to get the latest file version number of the specified
keyboard if it exists in the `assets/cloud/` or `assets/packages/`
folder.

If packageID is `cloud`, this method determines the latest file version
by the filename.

If packageID is something else, the metadata file
assets/packageID/kmp.json is parsed to determine the keyboard version.

## Examples

### Example: Using `getLatestKeyboardFileVersion()`

The following script illustrate the use of
`getLatestKeyboardFileVersion()`:

``` javascript
    String latestVersion = getLatestKeyboardFileVersion(this, "cloud", "tamil99m");
    if (latestVersion != null) {
        // If we assume that there are 2 tamil99m keyboard files in assets/cloud/ folder
        // with filenames; tamil99m-1.0.js and tamil99m-1.1.js
        // then latestVersion = "1.1"
    }
    else {
        // This keyboard does not exist in assets/cloud/ folder!
    }
```
