---
title: moveToElement  (deprecated)
---

## Summary

Move input focus to user specified element.

## Syntax

```js
keyman.moveToElement(Pelem);
```

### Parameters

`Pelem`
:   Type: `string|Element`
:   Moves focus to the user-specified element, finding it by element id if necessary.

### Return Value

`undefined`

## Description

Deprecation note: this function is no longer relevant, as touch-based input has
browser-level support with `inputmode=none`, so KeymanWeb no longer simulates
input fields. Use `Pelem.focus()` instead.

Previously, touch-based input worked through simulated input fields in KeymanWeb
rather than the original controls of the page. As a result, this function needed
to be used in place of the default `Pelem.focus()` method. It was used
internally to allow OSK-based iteration through controls.

## History

19.0: deprecated