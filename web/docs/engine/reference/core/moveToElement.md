---
title: moveToElement
---

## Summary

Move input focus to user specified element.

## Syntax

```c
keyman.moveToElement(Pelem);
```

### Parameters

`Pelem`
:   Type: `string|Element`
:   Moves focus to the user-specified element, finding it by element id if necessary.

### Return Value

`undefined`

## Description

Touch-based input works through simulated input fields in KeymanWeb
rather than the original controls of the page. As a result, this
function should be used in place of the default `Pelem.focus()` method.
It is used internally to allow OSK-based iteration through controls.
