---
title: KMManager.setDefaultKeyboard()
---

## Summary
The **setDefaultKeyboard()** method sets the keyboard information for the fallback keyboard.

## Syntax
```java
KMManager.setDefaultKeyboard(Keyboard keyboardInfo)
```

### Parameters
keyboardInfo

The keyboard information for the default keyboard.

## Description
The **setDefaultKeyboard()** method sets the keyboard information for the fallback keyboard. If Keyman Engine
has issues with a current keyboard, KMManager will switch to this fallback keyboard.

A fallback keyboard should also be defined if an app doesn't automatically add a keyboard (requires user selection).

**setDefaultKeyboard** should be called after KMManager.initialize().

## Examples

### Example: Using setDefaultKeyboard()
The following script illustrates the use of `setDefaultKeyboard()`: 
```java
    KMManager.initialize(getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);

    // Set the default (fallback) keyboard if this app doesn't automatically call addKeyboard().
    KMManager.setDefaultKeyboard(
        new Keyboard(
            "basic_kbdtam99", // Package ID - filename of the .kmp file
            "basic_kbdtam99", // Keyboard ID
            "Tamil 99 Basic", // Keyboard Name
            "ta",             // Language ID
            "Tamil",          // Language Name
            "1.0",            // Keyboard Version
            null,             // URL to help documentation if available
            "",               // URL to latest .kmp file
            true,             // Boolean to show this is a new keyboard in the keyboard picker
    
            KMManager.KMDefault_KeyboardFont,  // Font for the keyboard 
            KMManager.KMDefault_KeyboardFont)  // Font for OSK
    );
```

## History
Added syntax in Keyman Engine for Android 14.0.

## See also
* [getDefaultKeyboard](getDefaultKeyboard)
