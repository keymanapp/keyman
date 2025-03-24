---
title: stateMatch (KSM)
---

## Summary

State-key matching: Returns `true` if the event matches the rule's state-key requirements.

## Syntax

```c
keyman.interface.stateMatch(e, shiftCode);
```

or

```c
KeymanWeb.KSM(e, shiftCode); // Shorthand
```

### Parameters

`e`
:   Type: `Object`
:   A keystroke-related event object to match.

`state`
:   Type: `number`
:   The state-key bitflags the event should match.

### Return Value

`boolean`
:   `true` if the keystroke matches the desired states, otherwise `false`.

## Description

As of KeymanWeb 10.0, KeymanWeb now supports keyboard rules conditioned upon 'state keys' like Caps Lock [if specified by keyboard developers](/developer/language/guide/virtual-keys#toc-caps-lock). 

The `stateMatch` function examines the bit-flags to determine the state keys being tested and compares the keyboard event against them as appropriate. As such, it is designed to be a condition check alongside [`keyman.interface.keyMatch()`](keyMatch) and [`keyman.interface.contextMatch()`](contextMatch).

For comparison with [Developer 'rules'](/developer/language/guide/virtual-keys#toc-caps-lock) from keyboard source code, in the rule

```keyman
+ [CAPS K_A]  > 'BIG A'
+ [NCAPS K_A] > 'small a'
```

a keyboard would check which rule the triggering keystroke (`"a"`) matches by using

```c
keyman.interface.stateMatch(e, 0x100)
```

from within a raised keystroke event with object `e`, which checks that the keystroke represented by `e` has an activated Caps Lock state as represented by the code 0x0100. The NCAPS rule would instead compare against 0x0200.

As `keyman.interface.stateMatch()` receives an event object as one of its parameters, it does not need a direct link to the element receiving input.

## See also

- [`keyman.interface.keyMatch()`](keyMatch)
