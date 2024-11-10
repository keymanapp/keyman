---
title: osk.hide Event
---
  
## Summary

Called when the OSK is hidden.

## Syntax

```
keyman.osk.addEventListener('hide', function(param) {
  ...
});
```

### Parameters

`param`
:   Type: `Object`
:   An object with field HiddenByUser, a `boolean` which indicates if
    the OSK was hidden directly by the user's actions.

### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

...
