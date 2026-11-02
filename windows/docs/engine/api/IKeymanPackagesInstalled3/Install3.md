---
title: IKeymanPackagesInstalled3::Install3 Method
---

## Introduction

The `IKeymanPackagesInstalled3::Install3` method installs the package
file (.kmp) into Keyman Engine for Windows. This includes decompressing
the archive, copying its files into the Keyman Engine package store,
adding registry settings, installing fonts, and installing keyboard
layouts. During installation, keyboard layouts are compiled against the
specified base keyboard.

Fonts are copied and installed into the Windows Fonts folder, if an
existing font with the same name is not already present.

This method requires elevated privileges.

## Specification

``` clike
void Install3(string Filename, bool Force, int BaseKeyboardID)
```

## Parameters

`Filename`
:   The fully-qualified path to the .kmp file to be installed. This file
    will be copied to the Keyman package store.

`Force`
:   If `True`, overwrites an existing package entry.

`BaseKeyboardID`
:   The ID of the base keyboard to compile the package's keyboard layouts
    against.

## See also

[`IKeymanPackagesInstalled2::Install2`](../IKeymanPackagesInstalled2/Install2)
:   Installs a package, optionally with input method for default
    language

[`IKeymanPackagesInstalled3::Install3`](../IKeymanPackagesInstalled3/Install3)
:   Installs a package from a path and compiles its keyboard layouts
    against a specified base keyboard

[`IKeymanPackageFile3::Install3`](../IKeymanPackageFile3/Install3)
:   Installs a package file and compiles its keyboard layouts
    against a specified base keyboard
