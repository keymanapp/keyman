---
title: Keyman Package Metadata files
---

Used by:
: Keyman applications on all platforms except web.

Description:
: The files `kmp.inf` and `kmp.json` are metadata for a Keyman package file.

Details:

: When Keyman Developer compiles a Keyman package source file ([.kps](kps)), it
  creates and automatically adds `kmp.inf` and `kmp.json` into the Keyman
  Package file ([.kmp](kmp)).

  Keyman on all platforms now use `kmp.json` to install the keyboards
  ([.kmx](kmx)) in the package.

  Older versions of Keyman Desktop for Windows use `kmp.inf`, a parallel format
  of the package metadata. Note: the lexical model compiler does not add
  `kmp.inf`, and new functionality is not included. Future versions of Keyman
  Developer will stop supporting `kmp.inf`.

Package purposes:

: A Keyman Package file can be used to distribute keyboards (most common),
  lexical models, or user interface localizations for Keyman Desktop.

KMP.JSON
--------

### The package object

The kmp.json file is a base object described as

`system`

: `Object`

  [`System`](#obj-system) object.

`options`

: `Object`

  An [`Options`](#obj-options) object.

`startMenu`

: `Object`

  [`Start Menu`](#obj-startMenu) object.

`info`

: `Object`

  [`Info`](#obj-info) object.

`files`

: `Object`

  Array of objects, each containing a name and description of each file in in
  the package

`keyboards`

: `Object`

  Array of [`Keyboard`](#obj-keyboard) objects.

`lexicalModels`

: `Object`

  Array of [`LexicalModel`](#obj-lexicalModel) objects.

### The System object

The `System` object is used by Keyman Desktop to install keyboards

`keymanDeveloperVersion`

: `string`

  The version of Keyman Developer used to create the package file. If undefined,
  use `'0.0.0.0'`

`fileVersion,`

: `string`

### The Options object

The `Options` object is used by Keyman Desktop to install keyboards

`readmeFile`

: `string`

  A reference to the HTML file in the package for the keyboard package readme,
  presented before the package is installed.

`graphicFile`

: `string`

  A reference to an image file, .png or .jpeg recommended formats, associated
  with the keyboard package, and shown at package installation time.

`welcomeFile`

: `string`

  A reference to the HTML file containing keyboard package documentation,
  available through the help interfaces in Keyman, and shown once after the
  package is successfully installed. In the past, this file was always called
  welcome.htm.

`licenseFile`

: `string`

  A HTML file containing the package license.

### The Start Menu object

The `StartMenu` object is used by Keyman Desktop to install windows

`folder`

: `string`

  The folder that Keyman Desktop will create

`items`

: `Array`

  An array of [Item](#obj-item) objects

### The Item Object

The `Item` object

`name`

: `string`

  The item name

`filename`

: `string`

  The filename of the item

`location`

: `string`

  The location for Keyman Desktop to place the item

### The Info object

The `Info` object describes the Keyman package

`name`

: `string`

  The Keyman package name

`version`

: `string`

  The version number of the package in dotted number format. Defaults to `'1.0'` if missing

`copyright`

: `string` optional

  Copyright information

`author`

: `string` optional

  The Keyman package author and email address

`website`

: `string` optional

  Description and URL for additional Keyboard package documentation

### The Keyboard object

The `Keyboard` object describes an individual keyboard in the Keyman package. A package cannot contain both lexical models and keyboards.

`name`

: `string`

  Name of keyboard

`id`

: `string`

  ID of the keyboard, always matches the filename of the keyboard

`rtl`

: `boolean` optional

  `true` if the keyboard targets a right-to-left script. `false` if absent.

`version`

: `string`

  version number of the keyboard in dotted number format. Defaults to `'1.0'` if missing

`languages`

: `Array`

  An array of [`Language`](#obj-language) objects linked to the keyboard.

`displayFont`

: `string` optional

  The filename of the font for input fields (and OSK, if `oskFont` is not present).

`oskFont`

: `string` optional

  The filename of the font for the OSK

### The Language object

The `Language` object describes the language that can be typed with the keyboard

`name`

: `string`

  The name of the language

`id`

: `string`

  [BCP 47 language code](../bcp-47)

### The LexicalModel object

The `LexicalModel` object describes an individual model in the Keyman package. A package cannot contain both lexical models and keyboards.

`name`

: `string`

  Name of model

`id`

: `string`

  ID of the model, always matches the filename of the model

`rtl`

: `boolean` optional

  `true` if the model targets a right-to-left script. `false` if absent.

`version`

: `string`

  version number of the model in dotted number format.

`languages`

: `Array`

  An array of [`Language`](#obj-language) objects linked to the model.
