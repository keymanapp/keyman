---
title: BuildVisualKeyboard
---

## Summary

Create a copy of the OSK for embedding in documentation or help page.

## Syntax

```c
keyman.BuildVisualKeyboard(keyboardID, staticFlag, layoutFormFactor, layerID)
```

### Parameters

`keyboardID`
:   Type: `string`
:   Identifying name of the keyboard.

`staticFlag`
:   Type: `number`
:   Deprecated parameter; should be set to 1.

`layoutFormFactor`
:   Type: `string` *optional*
:   A string describing the layout to use for the generated keyboard. Should be one of the following:

    - `'desktop'`
    - `'phone'`
    - `'tablet'`

    Defaults to `'desktop'`.

`layerID`
:   Type: `string|number` *optional*
:   Name or index of the layer to be shown. If not specified, defaults to 'default', the base layer.

### Return Value

`Element`
:   A non-interactive DIV object will filled keyboard layer content.

## Description

Generates a non-interactive `<div>` element with the complete structure
of the specified layer of an on-screen keyboard (OSK) that the user can
then utilize for documentation or other purposes.
