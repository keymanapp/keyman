---
title: context (KC) (Deprecated)
---

## Summary

(Deprecated) Gets [`context`](/developer/language/reference/context) for an ongoing keyboard operation relative to the caret's present position.

## Syntax

```c
keyman.interface.context(n, ln, Pelem);
```

or

```c
KeymanWeb.KC(n, ln, Pelem); // Shorthand
```

### Parameters

`n`
:   Type: `number`
:   Relative position of the caret for the context retrieval operation.

`ln`
:   Type: `number`
:   Number of characters of text context to retrieve.

`Pelem`
:   Type: `Element`
:   The element being operated upon.

### Return Value

`string`
:   The requested [`context`](/developer/language/reference/context) text.

## Description

For an example from [Developer 'rules'](/developer/language/guide/rules), a keyboard might implement the following rule

```keyman
"abc" + "d" > context(2) "D"
```

by first checking that the initial context (`"abc"`) matches and then using the following to fulfill the rule:

```keyman
keyman.interface.output(3, Pelem, keyman.interface.context(2, 1, Pelem));
keyman.interface.output(0, Pelem, "D");
```

This operates by first replacing the original context `"abc"` with the requested subset of `context(2)`, then outputting the character (`"D"`) corresponding to the new keystroke.

## See also

- [`keyman.interface.contextMatch()`](contextMatch)
- [`keyman.interface.output()`](output)
