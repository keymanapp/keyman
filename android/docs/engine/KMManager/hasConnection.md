---
title: KMManager.hasConnection()
---

## Summary
The **hasConnection()** method returns whether the device has active network connection.

## Syntax
```java
KMManager.hasConnection(Context context)
```

## Parameters

`context`
: The context

## Returns
Returns `true` if application's AndroidManifest.xml file has granted 
[Manifest.permission.ACCESS_NETWORK_STATE](https://developer.android.com/reference/android/Manifest.permission#ACCESS_NETWORK_STATE) 
permission and the device has an active network connection, `false` otherwise.

## Description
Use this method to check if the device has an active network connection. It is important to make sure there is an active network connection before initiating a download or update.

## Examples

### Example: Using `hasConnection()`
The following script illustrate the use of `hasConnection()`:

```java
    if (KMManager.hasConnection(this)) {
        // has network connection
    }
    else {
        // no network connection
    }
```
