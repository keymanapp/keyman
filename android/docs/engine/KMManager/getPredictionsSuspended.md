---
title: KMManager.getPredictionsSuspended()
---

## Summary
The **getPredictionsSuspended()** method returns a flag that determines whether predictions are temporarily disabled because the currently selected text field is a hidden password text field or a numeric text field.

## Syntax
```java
KMManager.getPredictionsSuspended(KeyboardType keyboardType)
```

### Parameters

`keyboardType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

### Returns
Returns `true` if predictions are temporarily disabled because the currently selected text field is a hidden password text field or a numeric text field. `false` otherwise.

## Description
Use this method to check if predictions are temporarily disabled because of the type of currently selected text field. This value takes precedence over the user preference in the LanguageSettings menu.

## History
Added syntax in Keyman Engine for Android 18.0.

## See also
* [setPredictionsSuspended](setPredictionsSuspended)
