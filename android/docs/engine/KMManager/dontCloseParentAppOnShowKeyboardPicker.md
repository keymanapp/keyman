---
title: KMManager.dontCloseParentAppOnShowKeyboardPicker()
---

## Summary
The **dontCloseParentAppOnShowKeyboardPicker()** method disables the keyboard picker task flag so keyboard picker doesn't dismiss the parent app.

## Syntax

```javascript
KMManager.dontCloseParentAppOnShowKeyboardPicker()
```

## Description
Normally, launching the KeyboardPickerActivity from a system keyboard closes the parent app.

(The keyboard picker becomes the root of the activity stack)

Calling this function keeps the parent app running in the background.

## Examples

### Example: Using `dontCloseParentAppOnShowKeyboardPicker()`

The following script illustrates the use of `dontCloseParentAppOnShowKeyboardPicker()`:
```java
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    // When using Keyman system keyboard within the Keyman app, 
    // disable keyboard picker task flag so keyboard picker doesn't dismiss Keyman app
    KMManager.dontCloseParentAppOnShowKeyboardPicker();
```

## See also
* [closeParentAppOnShowKeyboardPicker()](closeParentAppOnShowKeyboardPicker)
* [showKeyboardPicker()](showKeyboardPicker)
