---
title: KMManager.addKeyboard()
---

## Summary

The **`addKeyboard()`** method adds a keyboard into the keyboards list.

## Syntax

``` javascript
KMManager.addKeyboard(Context context, Keyboard keyboardInfo)
```

### Parameters

`context`
:   The context.

`keyboardInfo`
:   A `Keyboard` type of keyboard information.

### Returns

Returns `true` if the keyboard was added successfully, `false`
otherwise.

## Description

Use this method to include a keyboard in the keyboards list so that it
can be selected from the keyboards menu. If the keyboard with same
keyboard ID and language ID exists, it updates the existing keyboard
info.

  

------------------------------------------------------------------------

## Syntax (Deprecated)

``` javascript
KMManager.addKeyboard(Context context, HashMap<String, String> keyboardInfo)
```

### Parameters

`context`
:   The context.

`keyboardInfo`
:   A dictionary of keyboard information with keys and values defined as
    `HashMap<String key, String value>`.

### Returns

Returns `true` if the keyboard was added successfully, `false`
otherwise.

## Description

Use this method to include a keyboard in the keyboards list so that it
can be selected from the keyboards menu. If the keyboard with same
keyboard ID and language ID exists, it updates the existing keyboard
info.

  

------------------------------------------------------------------------

## Examples

### Example: Using `addKeyboard()`

The following script illustrate the use of `addKeyboard()`:

``` javascript
    // Add a custom keyboard
    Keyboard kbInfo = new Keyboard(
      "basic_kbdtam99", // Package ID - filename of the .kmp file
      "basic_kbdtam99", // Keyboard ID - filename of the .js file
      "Tamil 99 Basic", // Keyboard Name
      "ta",             // Language ID
      "Tamil",          // Language Name
      "1.0",            // Keyboard Version
      null,             // URL to help documentation if available
      null,             // URL to latest .kmp file
      true,             // Boolean to show this is a new keyboard in the keyboard picker

      // Font information of the .ttf font to use in KMSample2 (for example "aava1.ttf").
      // basic_kbdtam99 doesn't include a font. Can set blank "" or KMManager.KMDefault_KeyboardFont
      // KMEA will use the font for the OSK, but the Android device determines the system font used for keyboard output
      KMManager.KMDefault_KeyboardFont,  // Font for KMSample2
      KMManager.KMDefault_KeyboardFont); // Font for OSK

    KMManager.addKeyboard(this, kbInfo);
```

## History

Added syntax using Keyboard type parameter in Keyman Engine for Android
14.0.

Deprecated syntax using the HashMap&lt;String key, String value&gt;
parameter in Keyman Engine for Android 14.0

## See also

-   [`removeKeyboard()`](removeKeyboard)
