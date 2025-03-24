---
title: IKeymanPackageFile::Install Method
---

## Introduction

The `IKeymanPackageFile::Install` method installs the package file
(.kmp) into Keyman Engine for Windows. This includes decompressing the
archive, copying the files contained into the Keyman Engine package
store, adding registry settings, installing fonts, and keyboard layouts
with default languages.

Fonts are copied and installed into the Windows Fonts folder, if an
existing font with the same name is not already present.

This method requires elevated privileges.

## Specification

``` clike
void Install(bool Force)
```

## Parameters

`Force`
:   If `True`, overwrites an existing package entry.

## See also

[`IKeymanPackagesInstalled::Install`](../IKeymanPackagesInstalled/Install)
:   Installs a package file and input method for default language

[`IKeymanPackagesInstalled::Install2`](../IKeymanPackagesInstalled2/Install2)
:   Installs a package file, optionally with input method for default
    language

[`IKeymanPackageFile2::Install2`](../IKeymanPackageFile2/Install2)
:   Installs a package file, optionally with input method for default
    language
