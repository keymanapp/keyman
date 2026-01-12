---
title: New LDML Keyboard Project Parameters Dialog
---

Allows you to quickly fill in common parameters for a new LDML keyboard project,
adding keyboard, package, documentation and metadata files, following the [file
layout](/developer/keyboards/) used in the [Keyman keyboards
repository](https://github.com/keymanapp/keyboards).

Projects can also be created from the command line with [kmc generate](kmc).

![Keyboard Editor - New file, Details tab](../images/ui/frmNewLDMLProjectParameters.png)

### Parameters

Keyboard Name

: **Required.** The name of the keyboard presented to end users. This will be
  set in the `<info name` attribute value in the keyboard, in the package name,
  and where appropriate in documentation and metadata.

Description

: **Required.** This field should describe your keyboard to people who find it
  online. It should explain the purpose of the keyboard in a friendly,
  descriptive way. The description could include details that will help users to
  decide if this keyboard is the appropriate choice for their needs.

: The first paragraph of the description will appear in the keyboard search
  result on keyman.com; the full description will be shown in the keyboard
  details page on keyman.com. Use
  [Markdown](https://www.markdownguide.org/basic-syntax) to include basic
  formatting, but not embedded HTML is not allowed.

: To add images to the description, you will need to reference an image on a
  server accessible online. For example, to use images from the Keyman
  keyboard repository where this keyboard can be uploaded, you can use the
  GitHub source. The URL starts with
  https://raw.githubusercontent.com/keymanapp/keyboards/master/ + the path to
  the image within the repository. For example:

  ```md
  ![GFF Mesobe Fidelat keyboard](https://raw.githubusercontent.com/keymanapp/keyboards/master/release/gff/gff_mesobe_fidelat/source/help/images/gff_mesobe_fidelat-default-1.jpeg)
  ```

  > [!IMPORTANT]
  > Images should not be included in the first paragraph.

  * Learn more about the [keyboard repository](https://help.keyman.com/developer/keyboards/)
  * The description can be edited later in the [Package Editor](package-editor).

Author

: Optional. The name of the developer of the keyboard. This will be set in
  the package metadata, and where appropriate in documentation and metadata.

Copyright

: Optional. A copyright string for the keyboard, excluding copyright year(s).
  This will be set in the package metadata, and where appropriate in
  documentation and metadata.

Full Copyright

: The same copyright string for the keyboard, including copyright year.

Version

: **Required.** The initial version number of the keyboard. For an LDML keyboard
  this can be a [SEMVER](https://semver.org) compatible version number, but note
  that Keyman as of version 17 is more restrictive and still requires the
  version number to follow the format specified in
  [`&Keyboardversion`](/developer/language/reference/keyboardversion). This will
  be set also in the package metadata, and where appropriate in documentation
  and metadata.

Supported Languages

: **Required.** Specifies the default BCP 47 language tags which will be added
  to the package data and project metadata. (Required) While the LDML keyboard
  specification allows for complete BCP 47 tags, in Keyman, these are currently
  restricted to use of the Language, Script, and Region subtags.

Path

: **Required.** Specifies the base path where the project folder will be
  created. The project folder name will be the keyboard ID. If the folder
  already exists, then you will be prompted before Keyman Developer overwrites
  files inside it.

Keyboard ID

: **Required.** The base filename of the keyboard, project and package. This
  must conform to the Keyman keyboard identifier rules, using the characters
  a-z, 0-9 and _ (underscore) only.