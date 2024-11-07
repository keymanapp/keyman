---
title: keyInformation (KKI)
---

## Summary

Returns an object with extended information about a specified keystroke event.

## Syntax

```c
keyman.interface.keyInformation(e);
```

or

```c
KeymanWeb.KKI(e); // Shorthand
```

### Parameters

`e`
:   Type: `Object`
:   The event object to be evaluated.

### Return Value

`Object`
:   An object with extended key event information.

## Description

---
**Note:** This function will not succeed if it is called from outside of a keyboard's standard text-processing operations.

---

The `key_event` object contains the following members:

`vk`
:   `boolean`
:   A flag indicating whether or not the event corresponds to a virtual key.

`code`
:   `number`
:   The underlying keycode for the event.

`modifiers`
:   `number`
:   A set of bit-flags corresponding to the SHIFT, CTRL, and ALT state of the keyboard.

The bit masks for each modifier are as follows:

-   SHIFT
:   `0x10`

-   CTRL
:   `0x20`

-   ALT
:   `0x40`
