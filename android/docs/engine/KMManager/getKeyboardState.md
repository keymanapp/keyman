---
title: KMManager.getKeyboardState()
---

## Summary

The **`getKeyboardState()`** method returns the specified keyboard's
state.

## Syntax

``` javascript
KMManager.getKeyboardState(Context context, String keyboardID, String languageID)
```

### Parameters

`context`
:   The context.

`keyboardID`
:   ID of the keyboard.

`languageID`
:   ID of the associated language.

### Returns

Returns the state of the specified keyboard as one of
`KEYBOARD_STATE_UNDEFINED`, `KEYBOARD_STATE_NEEDS_DOWNLOAD`,
`KEYBOARD_STATE_NEEDS_UPDATE` or `KEYBOARD_STATE_UP_TO_DATE`.

## Description

Use this method to get the state of the specified keyboard. It returns
`KEYBOARD_STATE_UNDEFINED` if keyboardID or languageID is not specified
(`null` or empty string). It returns `KEYBOARD_STATE_NEEDS_DOWNLOAD` if
the specified keyboard does not exist in `assets/languages/` folder. In
all other cases it returns either `KEYBOARD_STATE_UP_TO_DATE` or
`KEYBOARD_STATE_NEEDS_UPDATE`. Note: Only keyboards provided by Keyman
may return `KEYBOARD_STATE_NEEDS_UPDATE`. If you have a custom keyboard,
you need to implement your own method to check whether or not it needs
update. `KEYBOARD_STATE_UP_TO_DATE` does not necessarily mean that the
keyboard is in fact up to date, you need to make sure language list has
recently been displayed (see
[`showLanguageList() (Deprecated)`](showLanguageList)) without failure
to access Keyman server to be certain.

## Examples

### Example: Using `getKeyboardState()`

The following script illustrate the use of `getKeyboardState()`:

``` javascript
    KeyboardState keyboardState = KMManager.getKeyboardState(this, "tamil99m", "ta");
    switch (keyboardState) {
        case KEYBOARD_STATE_NEEDS_DOWNLOAD:
            // We need to download this keyboard or manually add it into assets/languages/ folder.
            break;
        case KEYBOARD_STATE_UP_TO_DATE:
        case KEYBOARD_STATE_NEEDS_UPDATE:
            // We can safely use this keyboard since it exists in assets/languages/ folder.
            break;
        default:
            // Undefined state
    }
```
