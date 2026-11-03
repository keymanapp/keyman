---
title: KPS files
---

Used by:
:   <span class="application">Keyman Developer</span>.

Description:
:   A .KPS file is a Keyman package source file.

Details:
:   A .KPS file is created using the Package Editor in
    <span class="application">Keyman Developer</span>. It specifies what
    files are to be included in the package. It is compiled into a
    Keyman Package file ([.KMP](kmp)).

Distributed with keyboard:
:   No. This is a development file and should not be distributed.

# File format

A keyboard package file contains one or more keyboards, along with readme files,
fonts (if your keyboard requires a custom font), and any other files you wish to
include. You should create a package file to bundle your keyboard with fonts and
help into a simple, single file that is easy for an end-user to install.

A lexical model package file contains a single lexical model, optionally also
fonts and/or documentation.

In addition to your own files, the package will contain a metadata file named
"kmp.json", which lists the details Keyman requires for installation.
The package file is a ZIP compatible archive.

> [!Tip] You can distribute keyboards and lexical models in package files, but
> you can't include both in the same package file.

## Schema

The schema for .kps is documented at:
https://github.com/keymanapp/keyman/tree/master/common/schemas/kps

## Strings
The optional `<Strings>` section of the file can be included to
customise the text in the bootstrap installer. The default strings are
found in the file:
[strings.xml](https://github.com/keymanapp/keyman/blob/stable-14.0/windows/src/desktop/setup/locale/en/strings.xml).
You can add your own strings for a given package which are used when
compiling as a bundled installer.

#### Keyman for Windows

Keyman for Windows can install keyboard package files, including installing
fonts, creating Start Menu shortcuts, and adding appropriate registry entries
for uninstallation.

On Windows, the context menu for a package file has one additional entry:
"Install".

## File.Name

The `Name` element may be either a local filename, or a GitHub raw permanent
URL, conforming to the pattern:

```
<Name>https://github.com/{owner}/{repo}/raw/{commit}/[path/]{filename}</Name>
```

## File.Source

The `<Source>` element documents the original source of the file, from
one of several supported sources:
* flo - fonts.languagetechnology.org
* github
* noto - in the future

While the `<Name>` element must refer to either a local file or to a permanent
GitHub file (as recognized by the presence of a commit hash), the `<Source>`
element typically will refer to a file which may change over time.

The `<Source>` element is supported in version 18.0 and later of .kps
format.

### Local file system

The `<Source>` element must not be included for files sourced from the local
file system, where `<Name>` references a file on the local filesystem.

Note: the `<Source>` element is optional for other sources as well; the absence
of the element does not mean that the `<Name>` element is necessarily
referencing a local file -- that is determined by pattern match.

```xml
<File>
  <Name>[relative-or-absolute-path/]{filename}</Name>
</File>
```

Note: absolute paths are not recommended because they are not portable to
other computers.

#### Example

```xml
<File>
  <Name>Alkalami-Regular.ttf</Name>
</File>
```

```xml
<File>
  <Name>../../shared/fonts/Alkalami-Regular.ttf</Name>
</File>
```

### fonts.languagetechnology.org `flo:<id>`

References a font by ID from fonts.languagetechnology.org. The resource must be
resolved to a permanent URL. If the `<Name>` field is missing, the compiler will
return an error, reporting the URL it discovers, so the author should be able to
paste that URL in.

If the URL in the `<Name>` field differs from the resolved URL given by FLO,
then a hint will be issued by the compiler, allowing the author to easily update
to a new version of the resource at their convenience. It is anticipated that
this may be scripted on the keyboards repository in the future.

```xml
<File>
  <Name>https://github.com/{owner}/{repo}/raw/{commit}/[path/]{filename}</Name>
  <Source>flo:{id}</Source>
</File>
```

#### Example

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>flo:alkalami</Source>
</File>
```

### GitHub

A reference to a resource located on GitHub. The resource must be resolvable to
a permanent URL. If the `<Name>` field is missing, the compiler will return an
error, reporting the URL it discovers, so the author should be able to paste
that URL in.

If the URL in the `<Name>` field differs from the resolved URL from the
`<Source>` element, then a hint will be issued by the compiler, allowing the
author to easily update to a new version of the resource at their convenience.
It is anticipated that this may be scripted on the keyboards repository in the
future.

```xml
<File>
  <Name>https://github.com/{owner}/{repo}/raw/{commit}/[path/]{filename}</Name>
  <Source>https://github.com/{owner}/{repo}/(raw|blob)/([refs/heads/]{branch}|[refs/tags/]{tag}|{commit})/[path/]{filename}</Source>
</File>
```

#### Examples

A GitHub raw reference, from a branch:

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/raw/main/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

Including `/refs/heads/` is also acceptable (as often found in GitHub UI):

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/raw/refs/heads/main/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

A GitHub blob reference, as copied from a URL in GitHub:

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/blob/main/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

A GitHub raw reference, from a commit, resolves identically. Note that as the
`<Source>` element includes a commit hash, the referenced file can never change
and so there is no real benefit in including it:

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

### noto

In a future version.

## Additional file format notes

The `CopyLocation`, `Description`, and `FileType` elements are deprecated as of
v18.0 and totally ignored by the compiler and end-user apps.

#### Keyman for macOS

Keyman for macOS can install package files with fonts and keyboards, and
shortcuts in the package will be available through the keyboard's entry in
Keyman Configuration.

#### Keyman for Linux

Keyman for Linux can install package files with fonts and keyboards.

#### Keyman for Android and Keyman for iPhone and iPad

Keyman mobile applications can install the same package files as Keyman for
Windows; the package must contain a touch-compatible keyboard (keyboard file
with a .js extension).

## Package file contents

A package can have a variety of different files contained within. The
following files and file types are recognized by the package installer:

\*.kmx (Keyman for Windows, Linux, and macOS only)
:   Keyboard files. Each of these will be installed. Keyman
    Configuration will not allow installation or uninstallation of a
    single keyboard from a package. They will always be treated as a
    group for installation and uninstallation.

\*.kvk (Keyman for Windows, Linux, and macOS only)
:   On Screen Keyboard files, associated with each keyboard file.

\*.js (mobile only)
:   Touch layout Keyboard files. When Keyman mobile applications install
    a keyboard package, all included keyboards will be installed as a
    group. With Keyman Developer 10+, the keyboard version information is
    in kmp.json, and no longer within the JS file names.

welcome.htm
:   Introductory help for the keyboard, HTML format. This will normally
    be displayed when the package is installed by the user, and is also
    the entry point for help when accessed via Keyman's help system or
    Keyman Configuration.

readme.htm
:   Displayed before a package is installed, together with brief
    metadata about the package, to allow the user to determine if they
    wish to continue installation of the package.

kmp.inf (required by Keyman 9 and earlier versions)
:   A Windows .ini format file that lists each of the files in the
    package, together with metadata.

kmp.json
:   A JSON format file containing metadata for the keyboard package such
    as package version, keyboard versions, and listing each of the files
    in the package. For more explanation of the structure of the JSON
    file, please read the [metadata](metadata) documentation.

\*.ttf, \*.otf, \*.ttc
:   Truetype font files that will be installed with the package, and
    uninstalled when the package is removed. On mobile, these fonts will
    be available only within the Keyman app and the on-screen-keyboard,
    not in other apps.

## Share the package file

> [!TIP]
> Read [File Layout](../file-layout) to know which files to include or exclude in the project folder.

The best way to distribute a package is to distribute it through Keyman Cloud,
which makes it available on keyman.com and in the Keyboard Download interface
inside the Keyman apps:

* [Share a keyboard package](/developer/keyboards)
* [Share a lexical model package](/developer/lexical-models)

It is also possible to share a .kmp package through other means. Once the
keyboard package .kmp file is created, you can share it via external storage
devices (USB drive, SD card, etc). You can also share the package file via a
cloud storage system (Google Drive, Dropbox or similar service), then share the
link to your device via text, note or email. Alternatively, you can upload the
.kmp file to a public facing website.