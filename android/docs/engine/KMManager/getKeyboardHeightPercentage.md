---
title: KMManager.getKeyboardHeightPercentage()
---

## Summary
The **getKeyboardHeightPercentage()** method returns the keyboard height as a percentage of the default height for the current or specified orientation.

## Syntax
```java
KMManager.getKeyboardHeightPercentage(Context context)
```
or

```java
KMManager.getKeyboardHeightPercentage(Context context, int orientation)
```

### Parameters

`context`
: The context

`orientation` _(Optional)_
: Accepts a [screen orientation](https://developer.android.com/training/multiscreen/screensizes#TaskUseOriQuali) value (Configuration.ORIENTATION_PORTRAIT or Configuration.ORIENTATION_LANDSCAPE). If not specified, uses the current device orientation.

### Returns
Returns an `int` representing the keyboard height as a percentage (e.g., 120 for 120%). The minimum return value is 100.

## Description
Use this method to determine how much the user has adjusted the keyboard height from its default size. The percentage is calculated using Math.ceil() to round up to the nearest integer, ensuring consistency with `createKeyboardHeightString()`.

For example, if the current height is 288px and the default height is 200px:
```
percentage = ceil(288 * 100.0 / 200) = ceil(144.0) = 144%
```

## Examples

### Example: Using `getKeyboardHeightPercentage()`
The following script illustrates the use of `getKeyboardHeightPercentage()`:

```java
    // Get the current keyboard height percentage
    int percentage = KMManager.getKeyboardHeightPercentage(this);
    Log.d("Keyboard", "Current height is " + percentage + "% of default");
```

or

```java
    import android.content.res.Configuration;
    ...
    // Get the keyboard height percentage for landscape orientation
    int landscapePercentage = KMManager.getKeyboardHeightPercentage(
        this,
        Configuration.ORIENTATION_LANDSCAPE
    );
```

## See also
* [getKeyboardHeight()](getKeyboardHeight)
* [getDefaultKeyboardHeight()](getDefaultKeyboardHeight)
* [applyKeyboardHeight()](applyKeyboardHeight)
* [createKeyboardHeightString()](createKeyboardHeightString)
