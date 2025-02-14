---
title: KMManager.setLongpressDelay()
---

## Summary

The `setLongpressDelay()` method sets the number of milliseconds to trigger a longpress gesture, applies to the keyboard, and stores preference.

## Syntax

```java
KMManager.setLongpressDelay(int longpressDelay)
```
### Parameter
`longpressDelay`
: The number of milliseconds, ranging from 300 ms to 1500 ms.

## Description
Use this method to store how many milliseconds to trigger a longpress as a preference.
This preference is stored at the Keyman app level, and is applied to all Keyman keyboards.

## Examples

### Example: Using `setLongpressDelay()`

The following code illustrates the use of `setLongpressDelay()`:
```java
    int currentDelayTimeMS = 300;

    // Store currentDelayTimeMS and applies
    KMManager.setLongpressDelay(currentDelayTimeMS);
```

## History
Keyman Engine for Android 18.0: New function.

## See also
* [getLongpressDelay](getLongpressDelay)
