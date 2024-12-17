---
title: deadkeyOutput (KDO)
---

## Summary

Deadkey output: Associates the [`deadkey`](/developer/language/reference/deadkey) state `dk` with the element at the current caret position, after overwriting `nd` characters.

## Syntax

```c
keyman.interface.deadkeyOutput(nd, Pelem, dk);
```

or

```c
KeymanWeb.KDO(nd, Pelem, dk); // Shorthand
```

### Parameters

`nd`
:   Type: `number`
:   The number of characters to overwrite (delete). May be set to `-1` or `0` to prevent overwrites.

`Pelem`
:   Type: `Element`
:   The element receiving output.

`dk`
:   Type: `number`
:   The deadkey id.

### Return Value

`undefined`

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

The Developer compiler then generates a unique id for the deadkey state - say, `0`, and upon detecting input of the `` '`' `` character with the [`keyman.interface.deadkeyMatch()`](deadkeyMatch) function, compiles the deadkey generation to

```c
keyman.interface.deadkeyOutput(0, Pelem, 0);
```

## See also

- [`keyman.interface.deadkeyMatch()`](deadkeyMatch)
- [Wikipedia article on deadkeys](https://en.wikipedia.org/wiki/Dead_key)
