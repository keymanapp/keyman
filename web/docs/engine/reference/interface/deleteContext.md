---
title: deleteContext (KDC)
---

## Summary

Context deletion - removes the specified number of deadkeys and characters from before the caret.

## Syntax

```c
keyman.interface.deleteContext(dn, Pelem);
```

or

```c
KeymanWeb.KDC(n, Pelem); // Shorthand
```

### Parameters

`dn`
:   Type: `number`
:   The number of entries (deadkeys, characters) to be deleted from the current context.

`Pelem`
:   Type: `Element`
:   The element being operated upon.

### Return Value

`undefined`

## Description

This is a core element of keyboard input management within KeymanWeb introduced with version 10. It is utilized to manage the deletion of context in a deadkey-aware manner, in parallel to [`keyman.interface.fullContextMatch`](fullContextMatch). For comparison with [Developer 'rules'](/developer/language/guide/rules) from keyboard source code...

```keyman
store(pair_1) 'uU'
store(pair_2) 'lL'

c Lots of keyboard rules...

'nul' dk(nothing) + '.' > nul
```

would have a rule output as follows:

```c
// Context is length four (three characters + one deadkey), so we delete all four.
keyman.interface.deleteContext(4, element);
```

## See also

- [`keyman.interface.fullContextMatch()`](fullContextMatch)
