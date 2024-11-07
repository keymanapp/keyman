---
title: setRect
---

## Summary

Set absolute position and size of desktop OSK window.

## Syntax

```c
keyman.osk.setRect(p);
```

### Parameters

`p`
:   Type: `Object`
:   An object specifying location and/or user permissions for altering the OSK's display.

### Return Value

`undefined`

## Description

The `setRect` object contains the following members:

`left`
:   `number` *optional*
:   Sets the x-coordinate of the OSK's left side.

`top`
:   `number` *optional*
:   Sets the y-coordinate of the OSK's top side.

`width`
:   `number` *optional*
:   Sets the width of the OSK.

`height`
:   `number` *optional*
:   Sets the height of the OSK.

`nosize`
:   `boolean` *optional*
:   If set to `true`, prevents the user from altering the OSK's size. If set to `false`, allows the user to resize the OSK. If omitted, does not change the existing state.

`nomove`
:   `boolean` *optional*
:   If set to `true`, prevents the user from relocating the OSK manually. If set to `false`, allows the user to move the OSK. If omitted, does not change the existing state.

The values and states of omitted object members will not be changed.

This differs from [`setPos()`](setPos) in that it allows resizing the keyboard and controlling the user's ability to move/size the keyboard. Furthermore, when `left` and `top` members are included, they override the default position of the keyboard rather than setting the position temporarily.
