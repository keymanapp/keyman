---
title: KMManager.getLongpressDelay()
---

## Summary

The `getLongpressDelay()` method returns from stored preference the number of milliseconds to trigger a longpress gesture.
Defaults to 500 milliseconds.

## Syntax

```java
int KMManager.getLongpressDelay()
```

### Returns
Returns the number of milliseconds to trigger a longpress gesture. This preference is stored at the app level and is applied to all Keyman keyboards.

## Description
Use this method to get details about how long to press a key for longpress keys to appear.

## Examples

### Example: Using `getLongpressDelay()`

The following code illustrates the use of `getLongpressDelay()`:
```java  
  int currentDelayTimeMS = KMManager.getLongpressDelay();

  currentDelayTimeMS += 250; // ms
```

## History
Keyman Engine for Android 18.0: New function.

## See also
* [sendOptionsToKeyboard](sendOptionsToKeyboard)
* [setLongpressDelay](setLongpressDelay)
