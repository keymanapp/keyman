---
title: getRect
---

## Summary

Get absolute position and size of OSK window.

## Syntax

```c
keyman.osk.getRect()
```

### Parameters

None.

### Return Value

`Object`
:   The position and size of the OSK's container element.

## Description

The returned `getRect` object contains the following members:

`left`
:   `number`
:   The x-coordinate corresponding to the left side of the OSK.

`top`
:   `number`
:   The y-coordinate corresponding to the top of the OSK.

`width`
:   `number`
:   The width (in pixels) of the OSK.

`height`
:   `number`
:   The height (in pixels) of the OSK.
