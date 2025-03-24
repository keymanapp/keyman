---
title: Distribute keyboards to Keyman Applications
---

## Overview

Keyboard package files contain one or more keyboards, along with readme
files, fonts (if your keyboard requires a custom font), and any other
files you wish to include. You should create a package file to bundle
your keyboard with fonts and help into a simple, single file that is
easy for an end-user to install.

Internally, as well as your own files, the package file will contain
metadata files "kmp.inf" and "kmp.json", which list the details Keyman
needs to install the package. The package file is a ZIP compatible
archive.

> ### Tip
You can distribute keyboards and lexical models in package files, but
you can't include both in the same package file.

#### Keyman Desktop

Keyman Desktop can install package files, including installing fonts,
creating Start Menu shortcuts, and adding appropriate registry entries
for uninstallation.

On Windows, the context menu for a package file has one additional
entry: "Install".

#### Keyman for macOS

Keyman for macOS can install package files with fonts and keyboards, and
shortcuts in the package will be available through the keyboard's entry
in Keyman Configuration.

#### Keyman for Android and Keyman for iPhone and iPad

Keyman mobile applications can install the same package files as Keyman
Desktop, as long as the package includes keyboards for touch layouts.

## Package file contents

A package can have a variety of different files contained within. The
following files and file types are recognized by the package installer:

\*.kmx (Desktop and macOS only)
:   Keyboard files. Each of these will be installed. Keyman
    Configuration will not allow installation or uninstallation of a
    single keyboard from a package. They will always be treated as a
    group for installation and uninstallation.

\*.kvk (Desktop and macOS only)
:   On Screen Keyboard files, associated with each keyboard file.

\*.js (mobile only)
:   Touch layout Keyboard files. When Keyman mobile applications install
    a keyboard package, all included keyboards will be installed as a
    group. With Keyman Developer 10, the keyboard version information is
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

kmp.inf (Legacy versions of Keyman)
:   A Windows .ini format file that lists each of the files in the
    package, together with metadata.

kmp.json
:   A JSON format file contains metadata for the keyboard package such
    as package version, keyboard versions, and lists each of the files
    in the package. For more explanation of the structure of the JSON
    file, please read the
    [metadata](../../reference/file-types/metadata) documentation.

\*.ttf, \*.otf, \*.ttc
:   Truetype font files that will be installed with the package, and
    uninstalled when the package is removed. On mobile, these fonts will
    be available only within the Keyman app and the on-screen-keyboard,
    not in other apps.

## Share the keyboard package file

> ### Tip
Read [Step 2: Organizing the Keyboard Files](../../../keyboards/github/step-2)
to know which files to include or exclude in the keybord folder. 

Once the keyboard package .kmp file is created, you can share them via
external storage devices (USB drive, SD card, etc). You can also share
the package file via a cloud storage system (Google Drive, Dropbox or
similar service), then share the link to your device via text, note or
email. Alternatively, you can upload the .kmp file to a public facing
website. For this example, the keyboard package for Khmer Angkor is
being uploaded:

1.  khmer_angkor.kmp (the keyboard package .kmp file)

## Putting the keyboard package on a website

Once all the files have been uploaded, you will need to provide a link
to the keyboard package .kmp file for your device to download and
install. This can either be a link on a web page, or a link in an email.
In this tutorial, a very simple .html web page with a link to the
khmer_angkor.kmp file is created:

``` markup
<html>
  <head>
  </head>
  <body>
    <a href="https://keyman.com/test/khmer_angkor.kmp">Custom Keyboard URL</a>
  </body>
</html>
```

The link must be in the format `http://` or `https://`

Upload the web page to your public facing website. Once done, you can
install the keyboard package onto your mobile devices by following these
steps:

-   [Installing custom keyboards to your iPhone or iPad](install-kmp-ios)
-   [Installing custom keyboards to your Android device](install-kmp-android)
