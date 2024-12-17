---
title: keyman.keyboardloaded Event
---

## Summary

Called when keyboard code loaded.

## Syntax

```
keyman.addEventListener('keyboardloaded', function(keyboardProperties) {
  ...
});
```

### Parameters

`keyboardProperties`
:   Type: `object`
:   An object with the following property:

: - `keyboardName` - the keyboard's name.

### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

This event is only raised once a keyboard has been fully loaded and
processed, ready for active use.
