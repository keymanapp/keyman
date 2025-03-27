---
title: KMManager.applyKeyboardHeight()
---

## Summary
The **applyKeyboardHeight()** method sets the height of the keyboard frame for 
the device's current [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) 
(portrait vs landscape) or a specified orientation.

## Syntax
```java
KMManager.applyKeyboardHeight(Context context, int height)
```
or

```java
KMManager.applyKeyboardHeight(Context context, int height, int orientation)
```

### Parameters

`context`
: The context

`height`
: The height of the keyboard frame in *density-independent pixels (dp)*. Pass `KMManager.KeyboardHeight_Reset` to reset the keyboard height (for current orientation) to device-specific defaults.

`orientation` _(Optional)_
: Accepts a [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) value. This is most useful if you want to change the size of the keyboard in the other (non-current) orientation. If `orientation` is not defined, keyboard height is set for the current device orientation.

## Description
Use this method when you want to increase or decrease the keyboard height for 
the device in the current screen orientation. This height is independent from 
the height of the suggestion banner frame.

For reference, here's a table of the default Keyman keyboard heights for various devices and screen orientation.
 
 Device Type and Screen Orientation | Default height (dp) |
|-----------------------------------|---------------------|
| Default handset in portrait | 205 |
| Default handset in landscape | 100 |
| 7" tablet in portrait | 305 |
| 7" tablet in landscape | 140 |
| 10" tablet in portrait | 405 |
| 10" tablet in landscape | 200 |

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

or

```java
    import android.content.res.Configuration;
    ...
    // Increase the Keyman keyboard height for landscape mode to 250dp (default Keyman value for most phones is 100dp)
    int newKeyboardHeight = 250;
    KMManager.applyKeyboardHeight(this, newKeyboardHeight, Configuration.ORIENTATION_LANDSCAPE);
```


The following script illustrates the use of `applyKeyboardHeight()` to reset keyboard height to default:

```java
    // Reset keyboard to default height
    KMManager.applyKeyboardHeight(this, KMManager.KeyboardHeight_Reset);
```

## See also
* [getKeyboardHeight()](getKeyboardHeight)
