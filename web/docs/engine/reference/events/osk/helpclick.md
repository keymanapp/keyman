---
title: osk.helpclick Event
---
  
## Summary

Called upon user request for the UI to present a help page.

## Syntax

```
keyman.osk.addEventListener('helpclick', function() {
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

This event is raised whenever the user wishes to access help
information.
