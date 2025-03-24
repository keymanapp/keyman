---
title: keyman.controlfocused Event
---

## Summary

Called when an input element receives focus.

## Syntax

```
keyman.addEventListener('controlfocused', function(eventProperties) {
  ...
});
```

### Parameters

`eventProperties`
:   Type: `object`
:   An object with the following properties:

: - `target` - the element gaining focus

: - `event` - the currently active control.

### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

This event is used by various KeymanWeb UI elements.
