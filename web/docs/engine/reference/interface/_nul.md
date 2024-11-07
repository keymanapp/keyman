---
title: nul (KN)
---

## Summary

[`nul`](/developer/language/reference/nul) context check: Returns `true` if the length of the [`context`](/developer/language/reference/context) is less than or equal to `n` characters.

## Syntax

```c
keyman.interface.nul(n, Pelem);
```

or

```c
KeymanWeb.KN(n, Pelem); // Shorthand
```

### Parameters

`n`
:   Type: `number`
:   The length of `context` to check.

`Pelem`
:   Type: `Element`
:   The element whose `context` is being examined.

### Return Value

`boolean`
:   `true` if the context is not longer than `n` characters, otherwise `false`.

## Description

This is of particular use in handling the [Developer rule `nul`](/developer/language/reference/nul) and is subject to the caveats listed therein.

## See also

- [`keyman.interface.context()`](context)
