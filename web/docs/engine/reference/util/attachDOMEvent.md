---
title: attachDOMEvent
---

## Summary

Attach user-defined event handler for any DOM event for an element.

## Syntax

```c
keyman.util.attachDOMEvent(Pelem, eventName, handler, useCapture);
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element to which the event is being attached.

`eventName`
:   Type: `string`
:   The name of the event, without an 'on' prefix.

`handler`
:   Type: `function(Object)`
:   The event handler being attached.

`useCapture`
:   Type: `boolean` *optional*
:   Set this to `true` if the event should be captured in the bubbling phase; otherwise, use `false` or leave this parameter empty.

### Return Value

`undefined`

## Description

The event name should be specified without the 'on' prefix. The fourth argument, useCapture, is optional and will default to false.

The method exists for legacy reasons that arise when dealing with old versions of IE, simplifying event-handling code.

See also: [`keyman.util.detachDOMEvent`](detachDOMEvent)
