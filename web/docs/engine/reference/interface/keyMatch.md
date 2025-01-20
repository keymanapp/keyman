---
title: keyMatch (KKM)
---

## Summary

Keystroke matching: Returns `true` if the event matches the rule's shift mask and key code.

## Syntax

```c
keyman.interface.keyMatch(e, shiftCode, keyCode);
```

or

```c
KeymanWeb.KKM(e, shiftCode, keyCode); // Shorthand
```

### Parameters

`e`
:   Type: `Object`
:   A keystroke-related event object to match.

`shiftCode`
:   Type: `number`
:   The shift-state code the event object should match.

`keyCode`
:   Type: `number`
:   The key code the event object should match.

### Return Value

`boolean`
:   `true` if the keystroke matches the desired values, otherwise `false`.

## Description

This is a core element of keyboard input management within KeymanWeb, typically called automatically during keystroke processing events. For comparison with [Developer 'rules'](/developer/language/guide/rules) from keyboard source code, in the rule

```keyman
"a" + "'" > "á"
```

a keyboard would check that the triggering keystroke (`"'"`) matches by using

```keyman
keyman.interface.keyMatch(e, 0, 39)
```

from within a raised keystroke event with object `e`, which checks that the keystroke represented by `e` is unshifted and matches the underlying keycode value for `"'"`, which is `39` in ASCII and Unicode.

As `keyman.interface.keyMatch()` receives an event object as one of its parameters, it does not need a direct link to the element receiving input.

## See also

- [`keyman.interface.contextMatch()`](contextMatch)
- [`keyman.interface.stateMatch()`](stateMatch)
