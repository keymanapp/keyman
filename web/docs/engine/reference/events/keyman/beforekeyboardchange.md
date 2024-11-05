---
title: keyman.beforekeyboardchange Event
---

## Summary

Called when keyboard input language is about to change.

## Syntax

```
keyman.addEventListener('beforekeyboardchange', function(keyboardProperties) {
  ...
});
```

### Parameters

`keyboardProperties`
:   Type: `object`
:   An object with the following properties:

: - `internalName` - the keyboard's name

: - `languageCode` - its BCP 47 language code.

### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

This event is designed to be used with UI modules.
