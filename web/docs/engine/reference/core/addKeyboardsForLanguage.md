---
title: addKeyboardsForLanguage function
---

## Summary

Add default or all keyboards for a given language to KeymanWeb.

## Syntax

```js
keyman.addKeyboardsForLanguage(spec[, spec...])
```

### Parameters

`spec`

: Type: `string`

  Language name string. Appending `$` to the language name will cause all available keyboards for that language to be loaded rather than the default keyboard.

### Return Value

`Promise`: A [JavaScript Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
fulfilled upon adding keyboards.

The promise is an array containing the following:
* successfully registered keyboard objects which define some or all of these [properties](../keyboard_properties)

## Description

The language spec is a string. Multiple language specs can also be
specified in a single call, which can reduce the round-trip cost of multiple
calls to Keyman Cloud servers (when using Keyman Cloud).

The first call to `addKeyboardsForLanguage()` makes an additional call to the Keyman API to load the current list of keyboard/language associations. This determines the default keyboards that are added for the language.

For general information and example uses of this method, please see the [Adding Keyboards](../../guide/adding-keyboards) page from the guide section.

### Using a `string`

For the given language specs, the Keyman Cloud is used to source the keyboard
file.
