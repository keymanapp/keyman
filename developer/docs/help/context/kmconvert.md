---
title: KMConvert Command Line Keyboard Conversion Utility
---

KMConvert generates keyboards and models from templates, and converts keyboard
layouts between different formats.

The [New Project dialog](new-project) in Keyman Developer provides a graphical
version of the functionality in KMConvert.

In a default installation of Keyman Developer on Windows, it is located in
`%ProgramFiles(x86)%\Keyman\Keyman Developer`, and should be on your `PATH`.
KMConvert will generate a full Keyman template project from the imported layout
or data, in a new folder named with the ID of the keyboard. It will not
overwrite an existing folder.

The following parameters are available:

```shell
kmconvert import-windows -klid <source-klid> [additional-options]
kmconvert template -id <keyboard_id> [additional-options]
kmconvert lexical-model -id-author <id-author> -id-language <id-language> -id-uniq <id-uniq> [additional-options]
```

### Available conversion modes

The conversion mode must be specified as the first parameter.

`import-windows`
: Imports a Windows keyboard into a new Keyman keyboard project. The `-klid`
parameter is required. This mode only functions on Windows platforms.

`template`
: Creates a blank keyboard project in the repository template format. The `-id`
parameter is required.

`lexical-model`
: Creates a wordlist lexical model project in the repository template format.
The `-id-author`, `-id-language`, and `-id-uniq` parameters are required.

### General parameters

`-author <data>`
: Name of author of the keyboard or model, no default.

`-copyright <data>`
: Copyright string for the keyboard or model, defaults to `Copyright (C)`,
  used everywhere except license file.

`-full-copyright <data>`
: Longer copyright string for the keyboard or model, defaults to
  `Copyright (C) yyyy`, used in license file.

`-languages <data>`
: Space-separated list of BCP 47 tags, e.g. `en-US tpi-PG`

`-name <data>`
: Name of the keyboard or model, e.g. `My First Keyboard`, `%s Basic`. Format
strings are only valid in `import-windows` mode; `%s` will be replaced with the
friendly English name of the imported keyboard.

`-nologo`
: Don't show the program description and copyright banner

`-o <destination>`
: The target folder to write the keyboard project into, defaults to `.\`

`-version <data>`
: Version number of the keyboard or model, defaults to `1.0`

### `template` and `import-windows` parameters

`-id <keyboard_id>`
: The id of the keyboard to create. Required for `template`, optional for
`import-windows`, not valid for `-lexical-model`. In `import-windows` mode, you
can use a format string with the placeholder `%s` which will be replaced with
the source filename, e.g. `kbdus`. If not specified, then the source filename
will form the id of the generated keyboard.

`-targets <data>`
: Space-separated list of [targets](/developer/language/reference/targets), e.g.
`linux windows phone`

### `import-windows` parameters

`-klid <source-klid>`
: The KLID of the keyboard to import, per LoadKeyboardLayout. This must be an
eight digit hex identifier, such as `00000409` for English (US), kbdus.dll. This
parameter is only valid for `import-windows` conversion mode.

### Lexical model parameters

`-id-author <data>`
: Identifier for author of model

`-id-language <data>`
: Single BCP 47 tag identifying primary language of model

`-id-uniq <data>`
: Unique name for the model

Note: model identifiers are constructed from parameters:
`<id-author>.<id-language>.<id-uniq>`

The values for `-id-author` and `-id-uniq` must be valid tokens: lower case a-z, 0-9, and underscore. These should also start with a letter or underscore.

### Examples

Create a lexical model for Tok Pisin:

```shell
kmconvert lexical-model -id-author james -id-language tpi-PG -id-uniq tokpisin -name "Tok Pisin" -languages tpi-PG
```

Create a keyboard for Javanese from a template:

```shell
kmconvert template -id sample -author "James Example" -name "My Javanese Keyboard" -copyright "Copyright (C) James Example" -full-copyright "Copyright (C) 2021 James Example" -languages jv-java
```

Create a new keyboard based on the Windows German keyboard:

```shell
kmconvert import-windows -klid 00000407 -id german_plus -name "German Plus" -copyright "Copyright (C) James Example" -full-copyright "Copyright (C) 2021 James Example" -author "James Example"
```
