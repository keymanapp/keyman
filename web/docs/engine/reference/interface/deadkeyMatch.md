---
title: deadkeyMatch (KDM) (Deprecated)
---

## Summary

(Deprecated)Deadkey matching: Seeks to match the [`deadkey`](/developer/language/reference/deadkey) state `dk` at the relative caret position `n`.

## Syntax

```c
keyman.interface.deadkeyMatch(n, Pelem, dk);
```

or

```c
KeymanWeb.KDM(n, Pelem, dk); // Shorthand
```

### Parameters

`n`
:   Type: `number`
:   The position to match, relative to the caret's present position.

`Pelem`
:   Type: `Element`
:   The HTML element receiving input.

`dk`
:   Type: `number`
:   The deadkey id.

### Return Value

`boolean`
:   `true` if the specified deadkey exists at the specified input location, otherwise `false`.

## Description

Deadkeys are useful for tracking hidden state information used to modify future keystrokes. For example, rather than using

```keyman
"`" + "a" = "à"
```

to combine two visible characters, certain applications may desire to keep the `` "`" `` character hidden with a rule such as

```keyman
+ '`' > dk(backquote)

...

dk(backquote) + "a" > "à"
```

The Developer compiler then generates a unique id for the deadkey state - say, `0`, and in order to detect the deadkey associated with the `` '`' `` character, compiles the `dk(backquote)` check to

```c
keyman.interface.deadkeyMatch(0, Pelem, 0)
```

which detects the existing deadkey (the second zero above) at the caret's present position (the first zero above).

For versions 10.0 and later, please consider use of [`fullContextMatch()`](fullContextMatch) instead.

## See also

- [`keyman.interface.deadkeyOutput()`](deadkeyOutput)
- [Wikipedia article on deadkeys](https://en.wikipedia.org/wiki/Dead_key)
