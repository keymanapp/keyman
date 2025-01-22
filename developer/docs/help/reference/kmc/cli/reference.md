---
title: kmc command line reference
---

kmc is the command line compiler toolset for Keyman 17.0 and later versions.
In Keyman Developer, it is located in `%ProgramFiles(x86)%\Keyman\Keyman Developer`.
kmc is also available as an npm module for Windows, macOS and Linux developers:

```
npm install -g @keymanapp/kmc
```

kmc does more than just compile keyboards. It builds packages, lexical models,
projects, keyboards, Windows installers, and more. It provides analysis tools
for keyboard data.

kmc will be extended to generate new keyboard and lexical model projects,
support renaming or cloning of existing projects, importing keyboard data from
other formats, and running keyboard unit tests.

kmc [replaces kmcomp](kmcomp-migration) from earlier versions of Keyman Developer.

The following parameters are available:

## `kmc` commands

`kmc build [infile...]`, `kmc build file [infile...]`

: Compile one or more Keyman files. Takes Keyman keyboard source files, and
  compiles them into the binary formats used by the Keyman apps. Supports
  building:
  * Keyman projects (.kpj or folder)
  * Keyman keyboards (.kmn)
  * LDML keyboards (.xml)
  * Keyboard packages - bundles of keyboard files, fonts, documentation for
    distribution (.kps)
  * Lexical models (.model.ts)

  The `file` subcommand is the default command for `kmc build`, so can be
  omitted.

  File lists can be referenced with @filelist.txt. If no input file is supplied,
  kmc will attempt to build a project file in the current folder.

`kmc build ldml-test-data`

: Converts LDML keyboard test .xml file to .json.

`kmc build windows-package-installer`

: Builds a .exe installer for a keyboard, together with the Keyman installer,
  for Windows only.

`kmc analyze osk-char-use [infile...]`

: Analyze on screen keyboard files for character usage

`kmc analyze osk-rewrite-from-char-use -m mapping-file [infile...]`

: Rewrites On Screen Keyboard files from source mapping

`kmc generate keyman-keyboard [options] <id>`

: Generate a .kmn keyboard project

`kmc generate ldml-keyboard [options] <id>`

: Generate an LDML .xml keyboard project

`kmc generate lexical-model [options] <id>`

: Generate a wordlist lexical model project

`kmc copy origin -o target`

: Copy and rename a keyboard project

`kmc message [message...]`

: Describes one or more compiler messages in greater detail

## `kmc` global options

`-h`, `--help`

: Display help on kmc; note that `kmc --help` can be used for further detail on
  subcommands, e.g. `kmc build --help`

`-V`, `--version`

: Prints the version number of kmc

`--no-error-reporting`, `--error-reporting`

: Enable or disable error reporting to keyman.com, overriding [user
  settings](../../user-settings). Error reporting is for fatal errors in the
  compiler, and not errors in compiled files. No user data is sent in error
  reports, although some filenames and paths may be present in the diagnostic
  data attached to the report.

`-l <logLevel>`, `--log-level <logLevel>`

: Controls the level of logging to console for messages relating to the
  compilation process. The options are:
  * `silent`: suppresses all messages (except fatal internal errors)
  * `error`: only emits compilation errors
  * `warn`: only emits compilation errors and warnings
  * `hint`: emits compilation errors, warnings, and hints
  * `info` (default): emits compilation errors, warnings, hints, and
    informational messages
  * `debug`: emits all compilation messages, plus internal debug messages

  Logging of specific [messages](messages) can be controlled with `--message`, `-m`.

  Note that when warnings are treated as errors
  (`--compiler-warnings-as-errors`, `-w`), they will still be logged as
  warnings, so suppressing warnings while using this flag may be confusing.

## `kmc build` options

`--color`, `--no-color`

: Controls colorization for log messages, using ANSI color controls. If both of
  these settings are omitted, kmc will attempt to detect from console, and will
  use colorization for interactive terminals, and no colorization when
  redirection is being used.

`-d`, `--debug`

: Include debug information in output files. Debug information is used for
  interactive debugging of .kmx files within the Keyman Developer IDE. This
  flag also produces pretty printed .js files for web keyboards, making
  interactive debugging of web keyboards simpler.

`-w`, `--compiler-warnings-as-errors` vs `-W`, `--no-compiler-warnings-as-errors`

: Controls whether or not warnings fail the build; overrides project-level
  warnings-as-errors option. Most compiler warnings are an indication that
  something is not right in the source code, even though the compiler can
  produce a result. This strict compilation mode helps to ensure that problems
  are caught early, and is recommended.

`-m <number>`, `--message <number>`

: Adjusts the severity of info, hint or warning messages. Error and fatal error
  messages can not be adjusted. Message severity can be adjusted to:
  * `disable` (default): suppresses the message altogether
  * `info`: converts the message to an informational severity
  * `hint`: converts the message to a hint severity
  * `warn`: converts the message to a warning severity
  * `error`: raises the message to an error severity

  This may be used to suppress a warning message that would otherwise fail the
  build, if used in conjunction with `-w`, `--compiler-warnings-as-errors`

  This option may be repeated to adjust multiple messages. The `-m` option must
  be specified each time.

`--no-compiler-version`

: Excludes compiler version metadata from output. This is helpful for producing
  files that will be identical regardless of the compiler version, for
  regression testing.

`--no-warn-deprecated-code`

: Turns off warnings (CWARN_HeaderStatementIsDeprecated,
  CWARN_LanguageHeadersDeprecatedInKeyman10) for deprecated code styles

`--log-format <logFormat>`

: Output log format. The available options are:
  * `formatted` (default): emits log messages in a human-readable format
  * `tsv`: emits log messages in UTF-8 tab-separated format. This format will be
    stable across versions of kmc. The format has the following fields:
    * filename
    * line number
    * severity
    * code
    * message

`-o <filename>`, `--out-file <filename>`

: Overrides the default path and filename for the output file(s). Note that
  some compilers emit multiple files, in which case, the output filenames
  will vary by file extension.

## `kmc build file` additional options

`--for-publishing`

: Verifies that project meets @keymanapp repository requirements. This also
  causes a .keyboard-info or .model-info file to be emitted when compiling the
  project (which can also be controlled at a project level with the
  `skipMetadataFiles` option). This option is only valid for compiling projects.

### Examples

```shell
kmc build project.kpj
```

Compile all components of a keyboard or model project named `project.kpj`.
kmc will respect the path settings within the project file. This is the
recommended way to build, as it will build keyboards, models and packages all in
one step. You can also call `kmc build <folder>` to build the project in the
referenced folder, e.g. `kmc build .`.

```shell
kmc build keyboard.kmn
```

Compile a keyboard file to a .kmx (desktop targets) and/or .js (web/touch
targets). If an output file is not specified, writes to the same folder as the
keyboard.

```shell
kmc build package.kps
```

Compile a package file to a .kmp (all targets). All included keyboards must
already be compiled.


## `kmc build windows-package-installer` additional options

`--msi <msiFilename>`

: Full path of keymandesktop.msi to bundle into the installer. This file can be
  downloaded from https://downloads.keyman.com/windows/stable (/version).

`--exe <exeFilename>`

: Location of setup.exe. This file can be downloaded from
  https://downloads.keyman.com/windows/stable (/version).

`--license <licenseFilename>`

: Location of license.txt for Keyman for Windows.

`--title-image [titleImageFilename]`

: Location of title image file. This should be a .png, .jpg, or .bmp file which
  replaces the standard 'Keyman for Windows' image in the bootstrap installer.

`--app-name [applicationName]`

: Installer property: name of the application to be installed (default: "Keyman")

`--start-disabled`

: Installer property: do not enable keyboards after installation completes

`--start-with-configuration`

: Installer property: start Keyman Configuration after installation completes

### Examples

#### Windows, command prompt (all one line)

```bat
kmc build windows-package-installer .\khmer_angkor.kps
  --msi "C:\Program Files (x86)\Common Files\Keyman\Cached Installer Files\keymandesktop.msi"
  --exe .\setup-redist.exe --license .\LICENSE.md --out-file .\khmer.exe
```

#### Bash (Linux, WSL, macOS, etc)

```shell
kmc build windows-package-installer \
  ./khmer_angkor/source/khmer_angkor.kps \
  --msi ./redist/keymandesktop.msi \
  --exe ./redist/setup-redist.exe \
  --license ./redist/LICENSE.md \
  --out-file ./khmer.exe
```

Note: paths shown above may vary.

## `kmc analyze osk-char-use` options

`-b, --base`

: First PUA codepoint to use, in hexadecimal (default F100)

`--include-counts`

: Include number of times each character is referenced (default: false)

`--strip-dotted-circle`

: Strip U+25CC (dotted circle base) from results (default: false)

`-m, --mapping-file <filename>`

: Result file to write to (.json, .md, or .txt)

`-i, --input-mapping-file <filename>`

: Merge result file with existing mapping file. If supplied, existing
  codepoint mappings will be kept, to ensure that updated fonts are
  backwardly compatible with deployed keyboards. The
  `--include-counts` flag will be set according to the format of
  the input mapping file.

For more information on the purpose of `analyze osk-char-use` and
`analyze rewrite-osk-from-char-use`, see
[`&displayMap`](/developer/language/reference/displaymap).

## `kmc analyze osk-rewrite-from-char-use` options

`-m, --mapping-file <filename>`

: JSON mapping file to read from.

For more information on the purpose of `analyze osk-char-use` and
`analyze rewrite-osk-from-char-use`, see
[`&displayMap`](/developer/language/reference/displaymap).

## `kmc generate keyman-keyboard` options

Generates a new keyboard project with .kmn format keyboard. An ID must be
specified for the output of the project, and an output folder (`-o`). A new
folder with the keyboard ID as its name will be created under the output folder,
and the files in that folder will follow the
[standard file layout](../../file-layout).

`-t, --target <target>`

: Target platforms for the project. Use the values from
  [`&targets`](/developer/language/reference/targets). Multiple targets may be
  specified, each prefixed with `-t`, or space-separated, surrounded by
  quotation marks (e.g. `-t windows -t linux` or `-t "windows linux"`) (default:
  `any`)

`-o, --out-path <path>`

: Specifies the parent folder for the project; the folder may already exist. The
  project will be generated in a new folder named with the ID of the project
  under this path, and that project folder must not exist.

`-n, --name <name>`

: Keyboard descriptive name, used in the
  [`&name` store](/developer/language/reference/name) (default: the ID of the
  project)

`-c, --copyright <copyright-name>`

: [`&copyright`](/developer/language/reference/copyright) holder for the
  project. Do not include the '(C)' or '&copy;' prefixes (default: the author
  of the keyboard)

`-v, --version <version-string>`

: [`&version`](/developer/language/reference/version) of the keyboard, (default:
  "1.0").

`-L, --language-tag <bcp-47 tag>`

: A BCP-47 language tag with which the keyboard is associated. More than one
  tag may be specified, with each tag prefixed with `-L` (default: no languages).
  The tags are referenced in the package metadata.

`-a, --author <author-name>`

: The name of keyboard author (default: blank)

<!-- TODO-GENERATE: uncomment once supported

`-i, --icon`

: Include a generated icon. The icon will be a 16x16 pixel box with the first
  letters of the first language tag (default: true, include an icon)
-->

`-d, --description <description>`

: A short description of the project, in Markdown. (default: keyboard name)

## `kmc generate ldml-keyboard` options

Generates a new keyboard project with LDML .xml format keyboard. An ID must be
specified for the output of the project, and an output folder (`-o`). A new
folder with the keyboard ID as its name will be created under the output folder,
and the files in that folder will follow the
[standard file layout](../../file-layout).

`-t, --target <target>`

: Target platforms for the project. Use the values from
  [`&targets`](/developer/language/reference/targets). Multiple targets may be
  specified, each prefixed with `-t`, or space-separated, surrounded by
  quotation marks (e.g. `-t windows -t linux` or `-t "windows linux"`) (default:
  `any`)

`-o, --out-path <path>`

: Specifies the parent folder for the project; the folder may already exist. The
  project will be generated in a new folder named with the ID of the project
  under this path, and that project folder must not exist.

`-n, --name <name>`

: Keyboard descriptive name, referenced in the keyboard `<info name` attribute
  (default: the ID of the project). Do not include generic terms such as
  'keyboard', 'unicode', or a version number in the name.

`-c, --copyright <copyright-name>`

: Copyright holder for the project. Do not include the '(C)' or '&copy;'
  prefixes (default: the author of the keyboard)

`-v, --version <version-string>`

: Version of the keyboard, referenced in the keyboard `<version number`
  attribute, should be in major.minor.patch format (note: full semantic version
  is not currently supported in Keyman) (defaults to "1.0.0")

`-L, --language-tag <bcp-47 tag>`

: A BCP-47 language tag with which the keyboard is associated. More than one
  tag may be specified, with each tag prefixed with `-L` (default: no languages).
  The first tag is referenced in the keyboard `<keyboard3 locale` attribute,
  with subsequent tags referenced in `<locale id` attributes. The tags are
  also referenced in the package metadata.

`-a, --author <author-name>`

: The name of keyboard author, referenced in `<info author` attribute (default:
  blank)

`-d, --description <description>`

: A short description of the project, in Markdown. (default: keyboard name)

## `kmc generate lexical-model` options

Generates a new lexical model project with a TSV wordlist. An ID must be
specified for the output of the project, and an output folder (`-o`). The ID
must match the [`author.bcp47.uniq`](/developer/lexical-models) naming pattern.
A new folder with the model ID as its name will be created under the output
folder, and the files in that folder will follow the
[standard file layout](../../file-layout).

`-o, --out-path <path>`

: Specifies the parent folder for the project; the folder may already exist. The
  project will be generated in a new folder named with the ID of the project
  under this path, and that project folder must not exist.

`-n, --name <name>`

: Lexical model descriptive name, referenced in the package metadata (default:
  the ID of the project)

`-c, --copyright <copyright-name>`

: Copyright holder for the project. Do not include the '(C)' or '&copy;'
  prefixes (default: the author of the model)

`-v, --version <version-string>`

: Version of the lexical model, referenced in the package metadata (defaults to
  "1.0")

`-L, --language-tag <bcp-47 tag>`

: A BCP-47 language tag with which the model is associated. More than one
  tag may be specified, with each tag prefixed with `-L` (default: no languages).
  The tags are referenced in the package metadata.

`-a, --author <author-name>`

: The name of model author, referenced in package metadata (default: blank)

`-d, --description <description>`

: A short description of the project, in Markdown. (default: lexical model name)

## `kmc copy` options

Copies a keyboard or lexical model project, renaming files matching the original
project ID according to the output filename. Can copy projects from the
following sources:

* A .kpj file, e.g. `./keyboards/khmer_angkor/khmer_angkor.kpj`, or `./sil.km.cnd.kpj`
* A local folder containing a .kpj file with a matching base name, e.g.
  `./keyboards/khmer_angkor` (which contains `khmer_angkor.kpj`), or
  `./models/sil.km.cnd/` (which contains `sil.km.cnd.kpj`)
* A cloud keyboard, or cloud lexical model, e.g. `cloud:khmer_angkor`. This
  retrieves the current source from the Keyman Cloud, which is in the GitHub
  repository keymanapp/keyboards or keymanapp/lexical-models. The project type
  will be determined by the id pattern -- either a lexical model
  `author.bcp47.uniq` id pattern, or a keyboard id pattern (where period `.` is
  not permitted)
* A GitHub repository or subfolder within a repository that matches the Keyman
  keyboard/model repository layout. For example,
  `https://github.com/keyman-keyboards/khmer_angkor/tree/main/khmer_angkor.kpj`

`-o, --out-path <filename>`

: The target folder to write the copied project. The folder must not exist.
  The folder basename will become the ID of the new project, so the .kpj,
  .kps, .kmn and similar files will be renamed to match that ID.

`-n, --dry-run`

: Show what would happen, without making changes

### File copying, renaming, and structure rules

The **origin** project folder is the one that contains the .kpj file.  When a
project is copied, referenced files are reorganized into the
[recommended Keyman project folder structure](../../file-layout). (Note the
difference between **origin** and `/source`: `/source` is a normal subfolder
in the recommended Keyman project folder structure).

The destination project is called the **target**.

* The **id** of the project and files can be updated during the copy. The
  **origin id** is the basename of the **origin** project file. The
  **target id** is supplied as the `-o` parameter, and becomes both the name of
  the output folder, and its basename becomes the basename of the **target**
  project. If other files use the same basename, they will also be updated.
* All source-type files explicitly referenced in **origin** .kpj will be copied
  to **target** `/source`, and references will be updated if the filename
  changes. These are the source-type files:
  * .kmn keyboard source
  * .xml LDML keyboard source
  * .kps package source
  * .model.ts model source
* Files referenced by source-type files will be copied to **target** folder
  structure, if they are also in the **origin** project folder. If the files are
  outside the **origin** folder, then relative references will be updated.
* File references in .html and other files are not tracked.
* For version 1.0 projects, only files explicitly referenced in the project or
  the source-type files are copied.
* For version 2.0 projects, all other files in the **origin** folder and
  subfolders will also be copied to **target**, in the same relative location as
  they were found in the **origin**. Files which have a **origin id** basename
  will also be renamed to use the **target id** basename (be aware that this
  could break untracked references).
* If a referenced file does not exist, for example the compiled files referenced
  in a .kps file may not be present, the references will still be updated
  following the rules above.
* Unreferenced files in the **origin** project's `build/` folder will not be
  copied.
* .kpj.user files will not be copied.
* .kpj options will be updated to use fixed `source/` and `build/` folders.

## `kmc message` options

One or more message identifiers can be specified for text or json formats.

Message identifiers are 5 hex digit codes, optonally preceded by `KM`, for
example `KM02001`.

For Markdown format, all messages are always emitted, to `out-path`, and no
messages can be specified on the command line, with one file per message, and
index files also generated. The Markdown mode is used to generate the online
documentation on help.keyman.com.

`-f, --format <format>`

: Output format, one of:
  * `text`: plain text output to console or file, of specified messages
  * `json`: JSON formatted to console or file, of specified messages
  * `markdown`: Markdown formatted text output, to a folder, of all messages

`-o, --out-path <out-path>`

: Output folder name for Markdown format (required for Markdown), or optional
  output filename for text and json formats.

`-a, --all-messages`

: Emit descriptions for all messages (text, json formats)

