---
title: keyman.keyboardchange Event
---

## Summary

Called when keyboard input language changed.

## Syntax

```
keyman.addEventListener('keyboardchange', function(keyboardProperties) {
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

This event is utilized by the various non-mobile UI elements included
with KeymanWeb.
