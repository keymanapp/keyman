---
title: any (KA) (Deprecated)
---

## Summary

(Deprecated) Returns whether or not the char `ch` is found within the [`any`](/developer/language/reference/any)([`store`](/developer/language/reference/store)) string, setting the internally-tracked index 'n' accordingly.

## Syntax

```c
keyman.interface.any(index, ch, storeText);
```

or

```c
KeymanWeb.KA(index, ch, storeText); // Shorthand
```

### Parameters

`index`
:   Type: `number`
:   The index id (starting from zero) at which the detected character will be internally tracked.

`ch`
:   Type: `string`
:   The input character to find in the text store.

`storeText`
:   Type: `string`
:   The contents of the specified text `store`.

### Return Value

`boolean`
:   `true` if the input character has a match in the text store, otherwise `false`.

## Description

This function corresponds most directly to the keyboard language command [`any`](/developer/language/reference/any), which operates upon text [`store`](/developer/language/reference/store)s to allow for array-based input-output text matching. For example,

```keyman
store(keys)   'abcde'
store(output) 'αβγδε'
+ any(keys) > index(output,1)
```

facilitates mapping the characters `'abcde'` to their respective entry in the output store `'αβγδε'`. `keyman.interface.any()`'s role in this process is usually optimized out into individual mappings for performance reasons, though it is often involved when `any` is involved as part of a rule's `context`, rather than upon the triggering keystroke itself.

In order to check if the character `'a'` has a match in the `keys` store above, the code

```c
keyman.interface.any(0, 'a', 'abcde')
```

would suffice, setting an internally-tracked index (id=0) to hold the index value 0. Note that the internally-tracked index (id=0) is decremented relative to the index appearing in the `index` call in the keyboard source above; the corresponding [`keyman.interface.indexOutput()`](indexOutput) call increments the id by 1 due to Keyman's keyboard language rules.

## See also

- [`keyman.interface.indexOutput()`](indexOutput)
