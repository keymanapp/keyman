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

For more information on the purpose of `analyze osk-char-use` and
`analyze rewrite-osk-from-char-use`, see
[`&displayMap`](/developer/language/reference/displaymap).

## `kmc analyze osk-rewrite-from-char-use` options

`-m, --mapping-file <filename>`

: JSON mapping file to read from.

For more information on the purpose of `analyze osk-char-use` and
`analyze rewrite-osk-from-char-use`, see
[`&displayMap`](/developer/language/reference/displaymap).

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

