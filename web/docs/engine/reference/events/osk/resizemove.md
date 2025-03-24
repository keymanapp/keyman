---
title: osk.resizemove Event
---
  
## Summary

Called when OSK resized or moved on desktop.

## Syntax

```
keyman.osk.addEventListener('resizemove', function() {
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

This event is raised whenever the OSK is moved.
