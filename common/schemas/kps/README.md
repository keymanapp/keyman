# kps.xsd

Master version: https://github.com/keymanapp/api.keyman.com/blob/master/schemas/kps/17.0/kps.xsd

## 2023-10-19 17.0
* Version 17.0 adds:
  - LicenseFile - a .md file, usually named LICENSE.md
  - WelcomeFile - a .htm file, usually named welcome.htm (later versions will support .md)
  - Info/Description - a short Markdown description of the content of the package, e.g. shown in search results on keyman.com
  - RelatedPackages - a list of other packages which relate to this one, or are deprecated by it
  - Keyboards/Keyboard/Examples - a list of typing examples for the keyboard
  - Keyboarsd/Keyboard/WebOSKFonts - a list of font filenames (not necessarily in package) suitable for rendering the on screen keyboard
  - Keyboarsd/Keyboard/WebDisplayFonts - a list of font filenames (not necessarily in package) suitable for use with the keyboard
* Version 17.0 removes:
  - LexicalModels/LexicalModel/Version - version information is not stored in the models, but only in the package metadata (was unused)

## 2023-04-21 7.0.1
* Removes LexicalModel.Version, as it was never read or written

## 2021-07-19 7.0
* Initial version 7.0

