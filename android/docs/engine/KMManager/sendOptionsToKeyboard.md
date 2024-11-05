---
title: KMManager.sendOptionsToKeyboard()
---

## Summary

The `sendOptionsToKeyboard()` method sends options like longpress delay to the KeymanWeb keyboard.

## Syntax

```java
KMManager.sendOptionsToKeyboard()
```

## Description
Use this method to update options in the KeymanWeb keyboard.
* Number of milliseconds to trigger a longpress gesture

This method requires a keyboard to be loaded for the values to take effect.

## Examples

### Example: Using `sendOptionsToKeyboard()`

The following code illustrates the use of `sendOptionsToKeyboard()`:
```java
    int currentDelayTimeMS = 250;

    // Store currentDelayTimeMS
    KMManager.setLongpressDelay(currentDelayTimeMS);

    // Apply the keyboard options
    KMManager.sendOptionsToKeyboard();
```

## History
Keyman Engine for Android 18.0: New function.

## See also
* [getLongpressDelay](getLongpressDelay)
* [setLongpressDelay](setLongpressDelay)
