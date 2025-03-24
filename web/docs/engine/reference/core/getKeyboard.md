---
title: getKeyboard
---

## Summary

Get keyboard meta data for the selected keyboard and language.

## Syntax

```c
keyman.getKeyboard(keyboardName, languageCode)
```

### Parameters

`keyboardName`
:   Type: `string`
:   The ID (internal name) of the keyboard.

`languageCode`
:   Type: `string` *optional*
:   The BCP 47 language code.

### Return Value

`Object`
:   An object with metadata corresponding to the requested keyboard.

## Description

The `keyboard` object contains the following members:

`Name`
:   `string`
:   User-friendly name of the keyboard.

`InternalName`
:   `string`
:   Internal name of the keyboard.

`LanguageName`
:   `string`
:   User-friendly name of the language actively tied to the keyboard.

`LanguageCode`
:   `string`
:   The three-letter code used to internally represent the language.

`RegionName`
:   `string`
:   The user-friendly name of the region of the world within which the language is predominantly found.

`RegionCode`
:   `string`
:   The three-letter code representing the region.

`CountryName`
:   `string` *optional*
:   The user-friendly name of the country in which the language is spoken.

`CountryCode`
:   `string` *optional*
:   A three-letter code corresponding to the country.

`KeyboardID`
:   `string` *optional*
:   *Deprecated.* A unique identifier for the keyboard. (Use 'InternalName' instead.)

`Font`
:   `string` *optional*
:   The font packaged with the keyboard to support its use.

`OskFont`
:   `string` *optional*
:   The font packaged with the keyboard to properly display specialized OSK characters.

## See also 
- [keyman.addKeyboards()](addKeyboards) and its documentation about keyboard specification objects.
