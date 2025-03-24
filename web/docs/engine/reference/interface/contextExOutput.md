---
title: contextExOutput (KCXO)
---

## Summary

`contextExOutput` function emits the character or object at `contextOffset` from the
current matched rule's context. Introduced in Keyman 14.0, in order to resolve a
gap between desktop and web core functionality for `context(n)` matching on `notany()`.

## Syntax

```js
  keyman.interface.contextExOutput(dn, outputTarget, contextLength, contextOffset);
```

or

```js
  KeymanWeb.KCXO(dn, outputTarget, contextLength, contextOffset); // Shorthand
```

## Parameters

`dn`
: Type: `number`
: number of characters to delete left of cursor

`outputTarget`
: Type: `OutputTarget`
: target to output to

`contextLength`
: Type: `number`
: length of current rule context to retrieve

`contextOffset`
: Type: `number`
: offset from start of current rule context, 1-based

## Returns

`void`

## See also

* [`keyman.interface.output()`](output)
* [`keyman.interface.indexOutput()`](indexOutput)

## History

* Introduced in Keyman 14.0
