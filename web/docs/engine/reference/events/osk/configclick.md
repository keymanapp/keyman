---
title: osk.configclick Event
---
  
## Summary

Called upon user request for the UI to present KeymanWeb configuration
options.

## Syntax

```
keyman.osk.addEventListener('configclick', function() {
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

This event is raised when the user wishes to access configuration
options.
