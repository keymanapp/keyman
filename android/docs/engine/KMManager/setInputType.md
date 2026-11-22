---
title: KMManager.setInputType()
---

## Summary
The **setInputType()** method allows the engine to configure which predictive-text features
will be presented to the user based upon the type of input expected for an input field.

## Syntax
```java
KMManager.setInputType(KeyboardType keyboardType, int inputType)
```

### Parameters

`keyboardType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

[`inputType`](https://developer.android.com/reference/android/text/InputType)
:   An integer defining the basic context type of text being edited.


## Description
Use this method to allow the engine to determine what types of suggestions may be offered to the
user.  It will disable predictions for contexts that should not provide them, such as password fields.
It will prevent autocorrect for contexts where text prediction will not likely give a perfect result,
such as email fields.

### Example: Using `setInputType()`
The following script illustrates the use of `setInputType()`:

```java
    KMManager.setInputType(textView.getInputType());
```

## History
Added syntax in Keyman Engine for Android 19.0.