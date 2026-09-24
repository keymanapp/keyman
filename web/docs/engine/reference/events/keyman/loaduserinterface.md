---
title: keyman.loaduserinterface Event
---

## Summary

Called after a KeymanWeb UI has been loaded and initialized.

## Syntax

```js
keyman.addEventListener('loaduserinterface', function() {
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

If no UI module has been loaded, then this event will not be called. If a UI is
loaded by script before KeymanWeb, it will be initialized at the end of
KeymanWeb's initialization sequence, but before the init promise resolves and
before [`initialized`](../../core/initialized.md) is set to `2`.
