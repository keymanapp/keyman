---
title: indexOutput (KIO)
---

## Summary

Index-based output: Outputs a mapped character according to a previous selection from a `keyman.interface.any()` call upon a [`store`](/developer/language/reference/store) string, after deleting `nd` characters.

## Syntax

```c
keyman.interface.indexOutput(nd, store, index, Pelem);
```

or

```c
KeymanWeb.KIO(nd, store, index, Pelem);
```

### Parameters

`nd`
:   Type: `number`
:   The number of characters to be overwritten.

`store`
:   Type: `string`
:   The store string to be used for mapping a character previously detected via `keyman.interface.any()`, which itself maps to a [`any`](/developer/language/reference/any) call.

`index`
:   Type: `number`
:   The index id to which the detected character's index in the original matching array was saved. The index id should be incremented by 1 relative to the value specified in `keyman.interface.any()`.

`Pelem`
:   Type: `Element`
:   The HTML element receiving the output.

## Description

This function corresponds most directly to the keyboard language command [`index`](/developer/language/reference/index), which operates upon text [`store`](/developer/language/reference/store)s to allow for array-based input-output text matching. For example,

```keyman
store(keys)   'abcde'
store(output) 'αβγδε'
+ any(keys) > index(output,1)
```

facilitates mapping the characters `'abcde'` to their respective entry in the output store `'αβγδε'`. `keyman.interface.indexOutput()`'s role in this process is usually optimized out into individual mappings for performance reasons, though it is often involved when `any` is involved as part of a rule's `context`, rather than upon the triggering keystroke itself.

In order to output the desired character corresponding to `'a'` in the `output` store above, the code

```c
keyman.interface.indexOutput(0, 'αβγδε', 1, Pelem)
```

would suffice, overwriting no characters and using the previously-matched index (id = 0 + 1). Note that the `keyman.interface.indexOutput()` call must increment the id by 1 due to Keyman's keyboard language rules, which corresponds directly to the `index` call in the keyboard source above.

## See also

- [`keyman.interface.any()`](any)
