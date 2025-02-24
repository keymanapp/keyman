---
title: keyman.keyboardregistered Event
---

## Summary

Called when a keyboard 'stub' is processed (for listing as available
keyboard).

## Syntax

```
keyman.addEventListener('keyboardregistered', function(keyboardProperties) {
  ...
});
```

### Parameters

`keyboardProperties`
:   Type: `object`
:   An object with the following properties:

: - `internalName` - the keyboard's id

: - `language` - its corresponding language.

: - `keyboardName` - the keyboard's name

: - `languageCode` - its BCP 47 language code.

### Return Value

`boolean`
:   `true` if the event should continue processing, `false` if it should
    not. Your event handler should return `true` aside from exceptional
    circumstances.

## Description

This event is used by all the UI elements in order to update the list of
available keyboards as soon as a new keyboard stub has been registered,
before any attempt to load it has been made.
