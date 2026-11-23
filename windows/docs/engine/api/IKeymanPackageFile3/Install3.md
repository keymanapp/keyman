---
title: IKeymanPackageFile3::Install3 Method
---

## Introduction

The `IKeymanPackageFile3::Install3` method installs the package file
(.kmp) into Keyman Engine for Windows. This includes decompressing the
archive, copying its files into the Keyman Engine package store, adding
registry settings, installing fonts, and installing keyboard layouts.
During installation, keyboard layouts are compiled against the specified
base keyboard.

Fonts are copied and installed into the Windows Fonts folder, if an
existing font with the same name is not already present.

This method requires elevated privileges.

## Specification

``` clike
void Install3(bool Force, int BaseKeyboardID)
```

## Parameters

`Force`
:   If `True`, overwrites an existing package entry.

`BaseKeyboardID`
:   The ID of the base keyboard to compile the package's keyboard layouts
    against.

## See also

[`IKeymanPackageFile2::Install2`](../IKeymanPackageFile2/Install2)
:   Installs a package, optionally with input method for default
    language

[`IKeymanPackagesInstalled3::Install3`](../IKeymanPackagesInstalled3/Install3)
:   Installs a package from a path and compiles its keyboard layouts
    against a specified base keyboard
