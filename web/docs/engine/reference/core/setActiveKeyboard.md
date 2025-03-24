---
title: setActiveKeyboard
---

## Summary

Change the currently active keyboard.

## Syntax

```c
keyman.setActiveKeyboard(keyboardName, languageCode);
```

### Parameters

`keyboardName`
:   Type: `string`
:   The ID (internal name) of the keyboard to be set as active.

`languageCode`
:   Type: `string` *optional*
:   The BCP 47 code for the keyboard's language.

### Return Value

`Promise`
:   A [JavaScript Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) fulfilled upon successfully linking and activating the keyboard.

## Description

Calls to `setActiveKeyboard` are asynchronous for keyboards not previously set as active.

Use the `internalName` and `languageCode` keyboard variables. If `languageCode` is defaulted, `setActiveKeyboard` will select the language code for the first matching keyboard stub.
