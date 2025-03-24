---
title: contextMatch (KCM) (Deprecated)
---

## Summary

(Deprecated) Context matching: Returns `true` if `context(n,ln,elem) == val`.

## Syntax

```keyman
keyman.interface.contextMatch(n, Pelem, val, ln);
```

or

```keyman
KeymanWeb.KCM(n, Pelem, val, ln); // Shorthand
```

### Parameters

`n`
:   Type: `number`
:   Relative position of the caret for the context match attempt.

`Pelem`
:   Type: `Element`
:   The element being operated upon.

`val`
:   Type: `string`
:   The desired text value for context to match.

`ln`
:   Type: `number`
:   Number of characters of text context to match.

### Return Value

`boolean`
:   `true` if the context matches the specified value `val`, otherwise `false`.

## Description

This is a core element of keyboard input management within KeymanWeb in versions prior to 10.0, typically called automatically during keystroke processing events. For comparison with [Developer 'rules'](/developer/language/guide/rules) from keyboard source code, in the rule

```keyman
"a" + "'" > "á"
```

a keyboard would check that the initial context (`"a"`) matches by using

```keyman
keyman.interface.contextMatch(1, Pelem, "a", 1)
```

which checks, starting at the first character to the left of the caret, a single character to see if it matches the value "a".

For versions 10.0 and later, please consider use of [`fullContextMatch()`](fullContextMatch) instead.

## See also

- [`fullContextMatch()`](fullContextMatch)
- [`keyman.interface.context()`](context)
- [`keyman.interface.output()`](output)
- [`keyman.interface.keyMatch()`](keyMatch)
