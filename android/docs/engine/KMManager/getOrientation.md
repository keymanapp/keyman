---
title: KMManager.getOrientation()
---

## Summary

The `getOrientation()` method returns the current orientation of the device.

## Syntax

```java
KMManager.getOrientation(Context context)
```

### Parameters

`context`
: The context.

### Returns
Returns the device orientation as an int, one of: 

* `Configuration.ORIENTATION_PORTRAIT` (`1`)
* `Configuration.ORIENTATION_LANDSCAPE` (`2`)
* `Configuration.ORIENTATION_UNDEFINED` (`0`)

## Description
Use this method to get the current orientation of the device

## Examples

### Example: Using `getOrientation()`

The following code illustrates the use of `getOrientation()`:
```java
    int orientation = KMManager.getOrientation(context);
```
