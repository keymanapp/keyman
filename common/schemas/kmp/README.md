# kmp.schema.json

* kmp.json file format, metadata included in Keyman .kmp package files

Documentation at https://help.keyman.com/developer/current-version/reference/file-types/metadata

# kmp.schema.json version history

## 2023-10-19 2.0
* Add relatedPackages, options.licenseFile, options.welcomeFile,
  keyboard.examples, keyboard.webOskFonts, keyboard.webDisplayFonts,
  info.description (all of these formerly were stored in .keyboard_info)

## 2019-01-31 1.1.0
* Add lexicalModels properties (note: `version` is optional and currently unused)

## 2018-02-13 1.0.2
* Add rtl property for keyboard layouts

## 2018-01-22 1.0.1
* Remove id field as it is derived from the filename anyway

## 2017-11-30 1.0 beta
* Initial version
