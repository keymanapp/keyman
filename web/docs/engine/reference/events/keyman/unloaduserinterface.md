---
title: keyman.unloaduserinterface Event
---

## Summary

Called when a KeymanWeb UI is about to be unloaded.

## Syntax

```js
keyman.addEventListener('unloaduserinterface', function() {
  ...
});
```

### Parameters

None.

### Return Value

`boolean`
:   `true` if the next event handler should be called, `false` if it should not.
    Your event handler should return `true` aside from exceptional
    circumstances.

## Description

Called when the ui is about to be unloaded, allowing cleanup of resources if
necessary.

It is not possible to stop the UI being unloaded; returning `false` only stops
further event handlers from being called.
