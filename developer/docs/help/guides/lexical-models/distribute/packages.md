---
title: Distribute lexical models to Keyman Applications
---

## Overview

Lexical model package files contain one lexical model, along with readme
files, and any other files you wish to include. You must create a
package file to bundle your lexical model and help documentation into a
simple, single file that is easy for an end-user to install.

The package file is a ZIP compatible archive.

> ### Tip
You can distribute keyboards and lexical models in package files, but
you can't include both in the same package file.

#### Keyman for Android and Keyman for iPhone and iPad

Keyman mobile applications can install the same lexical model package
files.

## Package file contents

A package can have a variety of different files contained within. The
following files and file types are recognized by the package installer:

\*.model.js
:   Lexical model file. When Keyman mobile applications install a
    lexical model package, the included model will be installed and
    associated with the specified languages.

welcome.htm
:   Introductory help for the lexical model, HTML format. This will
    normally be displayed when the package is installed by the user, and
    is also the entry point for help when accessed via Keyman's help
    system or Keyman Configuration.

## Step 1) Share the lexical model package file

Once the lexical model package .kmp file is created, you can share them
via external storage devices (USB drive, SD card, etc). If that is not
an option, you can upload the .kmp file to a public facing website. For
this example, the lexical model package for SENĆOŦEN is being uploaded:

1.  nrc.str.sencoten.model.kmp (the lexical model package .kmp file)

## Step 2) Create a link to the KMP file

Once all the files have been uploaded, you will need to provide a link
to the lexical model package .kmp file for your device to download and
install. This can either be a link on a web page, or a link in an email.
In this tutorial, a very simple .html web page with a link to the
nrc.str.sencoten.kmp file is created:

```html
<html>
  <head>
  </head>
  <body>
    <a href="https://keyman.com/test/nrc.str.sencoten.model.kmp">Sencoten Lexical Model Package</a>
  </body>
</html>
```

The link must be in the format `http://` or `https://`

Upload the web page to your public facing website. Once done, you can
install the lexical model package onto your mobile devices by
downloading the .kmp from your device's internet browser.