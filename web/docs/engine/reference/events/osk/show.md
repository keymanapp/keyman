---
title: osk.show Event
---
  
## Summary

Called when the OSK is displayed.

## Syntax

```
keyman.osk.addEventListener('show', function(obj) {
  ...
});
```

### Parameters

`obj`
:   Type: `object`
:   An object specifying the osk's display state information.

Available display state information:



`obj.x`
:   `Number`
:   The x-coordinate of the left side of the OSK.

`obj.y`
:   `Number`
:   The y-coordinate of the top of the OSK.

`obj.userLocated`
:   `boolean`
:   `true` if the OSK has been directly placed by the user.



### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

...
