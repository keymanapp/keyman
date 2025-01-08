---
title: restorePosition
---

## Summary

Move OSK back to default position, floating under active input element, for
desktop OSK.

## Syntax

```js
keyman.osk.restorePosition(keepDefaultPosition);
```

## Parameters

`keepDefaultPosition`
: `boolean` <span class="optional">optional</span>
: If `true` does not reset the default `left`, `top` set by [`setRect()`](setRect).
: If `false` or omitted, resets the default `left`, `top` as well.

## Return Value

`undefined`

## Description

This is the equivalent of pressing the pin in the title bar of the desktop OSK.
The user may move the OSK, after which point the OSK stays where the user has
placed it. Similarly, calling [`setPos()`](setPos) will fix the position of the
OSK. Calling `restorePosition()` resets the position of the OSK and re-attaches
it to the bottom of the active input element.

Note that if the user clicks the pin it does not reset the default `left`, `top`
as set by `setRect()`.

## History

* Published as public API in 14.0.272