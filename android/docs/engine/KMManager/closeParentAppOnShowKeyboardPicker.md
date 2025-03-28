---
title: KMManager.closeParentAppOnShowKeyboardPicker()
---

## Summary
The **closeParentAppOnShowKeyboardPicker()** method restores the default behavior of the keyboard picker menu closing the parent app.

## Syntax

```java
KMManager.closeParentAppOnShowKeyboardPicker()
```

## Description
Normally, launching the KeyboardPickerActivity from a system keyboard closes the parent app.

(The keyboard picker becomes the root of the activity stack.)

Calling this function restores this default behavior.

## Examples

### Example: Using `closeParentAppOnShowKeyboardPicker()`

The following script illustrates the use of `closeParentAppOnShowKeyboardPicker()`:
```java
  @Override
  protected void onResume() {
    super.onResume();
    KMManager.onResume();
    KMManager.hideSystemKeyboard();

    // Reset keyboard picker Activity Task flag
    KMManager.closeParentAppOnShowKeyboardPicker();
```

## See also
* [dontCloseParentAppOnShowKeyboardPicker()](dontCloseParentAppOnShowKeyboardPicker)
* [showKeyboardPicker()](showKeyboardPicker)
