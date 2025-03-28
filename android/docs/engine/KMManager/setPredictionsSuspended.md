---
title: KMManager.setPredictionsSuspended()
---

## Summary
The **setPredictionsSuspended()** method sets a flag to temporarily disable predictions when the currently selected text field is a hidden password text field or a numeric text field.

## Syntax
```java
KMManager.setPredictionsSuspended(int inputType, KeyboardType keyboardType)
```

### Parameters

[`inputType`](https://developer.android.com/reference/android/text/InputType)
:   An integer defining the basic context type of text being edited. If the text is one of the following InputTypes, this methods returns `true`:

* TYPE_CLASS_TEXT
* TYPE_TEXT_VARIATION_PASSWORD
* TYPE_CLASS_TEXT
* TYPE_TEXT_VARIATION_WEB_PASSWORD
* TYPE_CLASS_NUMBER
* TYPE_CLASS_PHONE

`keyboardType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

## Description
Use this method to temporarily disable predictions because of the currently selected text field. This value takes precedence over the user preference in the LanguageSettings menu.

## History
Added syntax in Keyman Engine for Android 18.0.

## See also
* [getPredictionsSuspended](getPredictionsSuspended)
