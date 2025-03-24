---
title: IKeymanKeyboardsInstalled2::Install2 Method
---

## Introduction

The `IKeymanKeyboardsInstalled2::Install2` installs a keyboard file
(.kmx) into Keyman Engine for Windows. This includes copying the file
into the Keyman Engine keyboard store, adding registry settings, and
optionally installing the Windows input method. As installing Windows
input methods is an asynchronous process, this method may not be
complete when it returns.

If the input method is installed, this method adds the keyboard to the
default language as specified in the
[`IKeymanKeyboard::DefaultBCP47Languages`](../IKeymanKeyboard/DefaultBCP47Languages)
property.

## Specification

``` clike
void Install2(string Filename, bool Force, bool InstallDefaultLanguage)
```

## Parameters

`Filename`
:   The fully-qualified path to the .kmx file to be installed. This file
    will be copied to the Keyman keyboard store.

`Force`
:   If `True`, overwrites an existing keyboard entry.

`InstallDefaultLanguage`
:   If `True`, installs the Windows input method for the default
    language.

## See also

[`IKeymanKeyboardsInstalled::Install`](../IKeymanKeyboardsInstalled/Install)
:   Installs a keyboard file and input method for default language

[`IKeymanKeyboardFile::Install`](../IKeymanKeyboardFile/Install)
:   Installs a keyboard file and input method for default language

[`IKeymanKeyboardFile2::Install2`](../IKeymanKeyboardFile2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language
