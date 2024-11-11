---
title: KMManager.getWindowDensity()
---

## Summary

The `getWindowDensity()` method returns the density of the window.

## Syntax

```java
KMManager.getWindowDensity(Context context)
```

`context`
: The context.

### Returns
Returns the density 

## Description
Use this method to get the [density](https://developer.android.com/reference/android/util/DisplayMetrics#density) of the window. This is a scaling factor for the Density Independent Pixel (DIP) unit. 

## Examples

### Example: Using `getWindowDensity()`

The following code illustrates the use of `getWindowDensity()`:
```java
    float density = KMManager.getWindowDensity(context);
```

## See Also
* [getWindowSize](getWindowSize)
