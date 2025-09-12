---
title: New Project Parameters Dialog
---

![New Project Parameters dialog](../images/ui/frmNewProjectParameters.png)

Allows you to quickly fill in common parameters for a new keyboard project,
adding keyboard, package, documentation and metadata files, following the
[file layout](/developer/keyboards/) used in the
[Keyman keyboards repository](https://github.com/keymanapp/keyboards).

Projects can also be created from the command line with
[kmc generate](kmc).

### Parameters

Keyboard Name

:   **Required.** The name of the keyboard presented to end users. This will be
    set in the [`&Name`](/developer/language/reference/name) store in the
    keyboard, in the package name, and where appropriate in documentation and
    metadata.

    Choose the keyboard name carefully; while it is possible to change it later,
    you will have to update the name in a number of locations.

Description

:   **Required.** This field should describe your keyboard to people who find it
    online. It should explain the purpose of the keyboard in a friendly,
    descriptive way. The description could include details that will help users
    to decide if this keyboard is the appropriate choice for their needs.

:   The first paragraph of the description will appear in the keyboard search
    result on keyman.com; the full description will be shown in the keyboard
    details page on keyman.com. Use
    [Markdown](https://www.markdownguide.org/basic-syntax) to include basic
    formatting, but not embedded HTML is not allowed.

:   To add images to the description, you will need to reference an image on a
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

:   The name of the developer of the keyboard. This will be set in the package
    metadata, and where appropriate in documentation and metadata.

Copyright

:   A copyright string for the keyboard, excluding copyright year(s). This will
    be set in the [`&Copyright`](/developer/language/reference/copyright) store
    in the keyboard, in the package metadata, and where appropriate in
    documentation and metadata.

Full Copyright

:   The same copyright string for the keyboard, including copyright year.

Version

:   **Required.** The initial version number of the keyboard. This should follow
    the format specified in the
    [`&Keyboardversion`](/developer/language/reference/keyboardversion) store
    for the keyboard. This will be set also in the package metadata, and where
    appropriate in documentation and metadata.

Targets

:   **Required.** Specifies the default deployment targets for the keyboard, set
    in the [`&Targets`](/developer/language/reference/targets) store in the
    keyboard, and controls the files added to the package initially. This also
    is reflected in documentation and metadata.

Languages

:   Specifies the default BCP 47 language tags which will be added to the
    package metadata and project metadata.

Path

:   **Required.** Specifies the base path where the project folder will be
    created. The project folder name will be the keyboard ID. If the folder
    already exists, then you will be prompted before Keyman Developer overwrites
    files inside it.

Keyboard ID

:   **Required.** The base filename of the keyboard, project and package. This
    must conform to the Keyman keyboard identifier rules, using the characters
    a-z, 0-9 and _ (underscore) only.