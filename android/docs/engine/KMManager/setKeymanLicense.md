---
title: KMManager.setKeymanLicense() (Deprecated)
---

## Summary

(Deprecated) The **`setKeymanLicense()`** method sets the developer
license/key pair to unlock Keyman Engine.

## Syntax

``` javascript
KMManager.setKeymanLicense(String license, String key)
```

### Parameters

`license`
:   Your developer license for Keyman Engine for Android.

`key`
:   Your developer key for Keyman Engine for Android.

## Description

You must use this method to set the developer license/key pair before
initializing the KMManager. You will receive the license/key pair when
you purchase Keyman Engine for Android.

## Examples

### Example: Using `setKeymanLicense()`

The following script illustrate the use of `setKeymanLicense()`:

``` javascript
    KMManager.setLicense(“YourLicense”,”YourKey”);
    // Initialize KMManager here after setting the license
```

## History

Deprecated in Keyman Engine for Android 12.0

## See also

-   [`KMManager.initialize()`](initialize)
