---
title: KMManager.applyKeyboardHeight()
---

## Summary
The **applyKeyboardHeight()** method sets the height of the keyboard frame for 
the device's current [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) 
(portrait vs landscape).

## Syntax
```java
KMManager.applyKeyboardHeight(Context context, int height)
```

### Parameters

`context`
: The context

`height`
: The height of the keyboard frame in *density-independent pixels (dp)*

## Description
Use this method when you want to increase or decrease the keyboard height for 
the device in the current screen orientation. This height is independent from 
the height of the suggestion banner frame.

For reference, here's a table of the default Keyman keyboard heights for various devices and screen orientation.
 
 Device Type and Screen Orientation | Default height (dp) |
|-----------------------------------|---------------------|
| Default handset in portrait | 205 |
| Default handset in landscape | 150 |
| 7" tablet in portrait | 305 |
| 7" tablet in landscape | 200 |
| 10" tablet in portrait | 405 |
| 10" tablet in landscape | 300 |

**Note:** This new keyboard height would be applied for all platforms, so an 
adjusted keyboard height for a phone would appear too small for a tablet.

## Examples

### Example: Using `applyKeyboardHeight()`
The following script illustrates the use of `applyKeyboardHeight()`:

```java
    // Increase the Keyman keyboard height (default Keyman value for most phones is 205dp)
    int newKeyboardHeight = 300;
    KMManager.applyKeyboardHeight(this, newKeyboardHeight);
```

## See also
* [getKeyboardHeight()](getKeyboardHeight)
