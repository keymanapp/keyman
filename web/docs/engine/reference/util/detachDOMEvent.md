---
title: detachDOMEvent
---

## Summary

Detach DOM event handler from element to prevent memory leaks.

## Syntax

```c
keyman.util.detachDOMEvent();
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element from which the event is being detached.

`eventName`
:   Type: `string`
:   The name of the event, without an 'on' prefix.

`handler`
:   Type: `function(Object)`
:   The event handler being detached.

`useCapture`
:   Type: `boolean` *optional*
:   Set this to `true` if the event was being captured in the bubbling phase; otherwise, use `false` or leave this parameter empty.

### Return Value

`undefined`

## Description

The fourth argument, useCapture, is optional and will default to false.

See also: [`keyman.util.attachDOMEvent`](attachDOMEvent)
