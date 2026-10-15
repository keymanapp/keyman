---
title: getActiveLanguage
---

## Summary

Get the language tag or name for the currently selected language for the active
keyboard.

## Syntax

```js
keyman.getActiveLanguage(fullName)
```

### Parameters

`fullName`
:   Type: `boolean` *optional*
:   If true, return the language name

### Return Value

`string`
:   The active language's BCP 47 language tag or name.

## Description

The BCP 47 language tag returned relates to the currently active registered
keyboard. If no keyboard is active, the return value will be an empty string.

The language tag may be any well-formed
[BCP 47](/developer/current-version/reference/bcp-47) language tag.

The language name is also derived from the keyboard registration, so may not
match language names from `Intl` or other sources.