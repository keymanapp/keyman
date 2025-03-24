---
title: keyman.unloaduserinterface Event
---

## Summary

Called when allow ui clean-up.

## Syntax

```
keyman.addEventListener('unloaduserinterface', function() {
  ...
});
```

### Parameters

None.

### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

Called when the ui is to be unloaded, allowing cleanup of resources if
necessary.
