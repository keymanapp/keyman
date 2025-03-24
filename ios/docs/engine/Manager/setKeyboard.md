---
title: Manager.setKeyboard()
---

## Summary

The **`setKeyboard()`** method sets the current keyboard to be used, querying the user's list of keyboards.

## Syntax

``` swift
Manager.setKeyboard(fullID: FullKeyboardID)
```

### Parameter

`fullID`
:   ID of the keyboard.

### Returns

Returns `true` if the keyboard was set successfully, `false` otherwise.

## Description

This syntax can be used for setting a keyboard which is available on the
Keyman server. Make sure language list is displayed (see
[`showKeyboardPicker()`](showKeyboardPicker)) at least once before using
this syntax, otherwise it may fail to set the keyboard properly. Do not
use this syntax to set a custom keyboard.

## Examples

### Example 1: Using `setKeyboard()`

The following script illustrates the use of `setKeyboard()`:

``` swift
    // Setting a Keyman keyboard
    Manager.setKeyboard("sil_euro_latin");
```

------------------------------------------------------------------------

## Syntax

``` swift
Manager.setKeyboard(kb: InstallableKeyboard)
```

### Parameter

`kb`
:   Information for the keyboard to set.

### Returns

Returns `true` if the keyboard was set successfully, `false` otherwise.

## Description

This syntax can be used for setting a keyboard which is available on the
Keyman server. Make sure language list is displayed (see
[`showKeyboardPicker()`](showKeyboardPicker)) at least once before using
this syntax, otherwise it may fail to set the keyboard properly. Do not
use this syntax to set a custom keyboard.

## Examples

### Example 1: Using `setKeyboard()`

The following script illustrates the use of `setKeyboard()`:

``` swift
for keyboard in keyboards {
  Manager.shared.addKeyboard(keyboard)
  Manager.shared.setKeyboard(keyboard)
}
```

## See also

-   [`addKeyboard()`](addKeyboard)
-   [`switchToNextKeyboard()`](switchToNextKeyboard)
