---
title: addKeyboards function
---

## Summary

Adds keyboards to KeymanWeb.

## Syntax

```js
keyman.addKeyboards(spec[, spec...])
```

### Parameters

`spec`

: Type: `string|Object`

  keyboard name string or keyboard metadata JSON object

### Return Value

`Promise`: A [JavaScript Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
fulfilled upon adding keyboards.

The promise is an array containing a combination of the following:
* successfully registered keyboard objects which define some or all of these [properties](../keyboard_properties)
* [ErrorStub](../keyboard_registration_errors) objects for keyboards that failed to register

## Description

The keyboard spec can be a string or an object. Multiple keyboard specs can be
specified in a single call, which can reduce the round-trip cost of multiple
calls to Keyman Cloud servers (when using Keyman Cloud).

For general information and example uses of this method, please see the [Adding Keyboards](../../guide/adding-keyboards) page from the guide section.

### Using a `string`

When keyboard spec is a string, the Keyman Cloud is used to source the keyboard
file. Keymanweb will load webfonts and keyboard file automatically on demand.
The string format is one of the following:

* `'keyboardID'`: Adds a specific keyboard, linking it to the default language for the keyboard
* `'@languageID'`: Adds the default keyboard for the specified BCP 47 language code
* `'keyboardID@languageID'`: Loads a specific keyboard + language combination

The keyboard catalogue is online at
[http://keyman.com/developer/keymanweb/keyboards](http://keyman.com/developer/keymanweb/keyboards).

### Using an `object`

When keyboard spec is an object, then more parameters can be specified,
including sourcing the keyboard from locations other than the Keyman Cloud. The
specification of the object is related to the Keyman Cloud JSON API (formerly
known as KeymanWeb Server Data API):

The `spec` object contains the following members:


`name`

: `string`

  Name of the keyboard.

`id`

: `string`

  ID (internal name) of keyboard, together with version always matches the
  filename of the keyboard

`filename`

: `string`

  url of keyboard *.js file, relative (to page) or absolute

`languages`

: `array|object`

  An array of objects (see definition below) or single object linked to the
  keyboard.

`rtl`

: `boolean` <span class="optional">optional</span>

  `true` if the keyboard targets a right-to-left script. May be set to `false`
  or left `undefined` otherwise.

`version`

: `string` <span class="optional">optional</span>

  Version of keyboard *.js file

`displayName`

: `string` <span class="optional">optional</span>

  A name to display in the spacebar of the keyboard; if omitted, use the
  default [`spacebarText` option](init#init_options).

---

The `spec.languages` object contains the following members:

`name`

: `string`

  Name of the language.

`id`

: `string`

  BCP 47 language code.

`region`

: `string` <span class="optional">optional</span>

  _Required_ when using the Toolbar UI. May be set with one of the following
  region names:

  * `'World'` (`'un'`)
  * `'Africa'` (`'af'`)
  * `'Asia'` (`'as'`)
  * `'Europe'` (`'eu'`)
  * `'South America'` (`'sa'`)
  * `'North America'` (`'na'`)
  * `'Oceania'` (`'oc'`)
  * `'Central America'` (`'ca'`)
  * `'Middle East'` (`'me'`)

`font`

: `array|object` <span class="optional">optional</span>

  An array of Font objects (see definition below) or single object describing
  fonts for input fields and the OSK (if `oskFont` is not present.)

`oskFont`

: `array|object` <span class="optional">optional</span>

  An array of Font objects (see `font` definition below) or single object
  describing fonts for the OSK.

---

The `spec.languages.font` object contains the following members:

`family`

: `string`

  Font family that KeymanWeb will provide for this font.

`filename`

: `array`

  Array of URLs where font resources can be accessed, relative to the [`fonts`
  initialization property](init). Multiple font resources can be specified as
  platform font format support varies and KeymanWeb will pick the most
  appropriate for the platform.

`size`

: `string` <span class="optional">optional</span>

  Font size (in CSS dimensions). If not specified, then `1em` is used.
