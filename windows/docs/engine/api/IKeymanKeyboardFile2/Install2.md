---
title: IKeymanKeyboardFile2::Install2 Method
---

## Introduction

The `IKeymanKeyboardFile2::Install2` method installs the keyboard file
(.kmx) into Keyman Engine for Windows. This includes copying the file
into the Keyman Engine keyboard store, adding registry settings, and
optionally installing the Windows input method. As installing Windows
input methods is an asynchronous process, this method may not be
complete when it returns.

This method adds the keyboard to the default language as specified in
the
[`IKeymanKeyboard::DefaultBCP47Languages`](../IKeymanKeyboard/DefaultBCP47Languages)
property. This method is the same as calling
[`IKeymanKeyboardsInstalled::Install`](../IKeymanKeyboardsInstalled/Install)
and passing the
[`IKeymanKeyboard::Filename`](../IKeymanKeyboard/Filename) property from
this instance.

This method requires elevated privileges.

## Specification

``` clike
void Install(bool Force, bool InstallDefaultLanguage)
```

## Parameters

`Force`
:   If `True`, overwrites an existing keyboard entry.

`InstallDefaultLanguage`
:   If `True`, installs the Windows input method for the default
    language.

## See also

[`IKeymanKeyboardsInstalled::Install`](../IKeymanKeyboardsInstalled/Install)
:   Installs a keyboard file and input method for default language

[`IKeymanKeyboardsInstalled::Install2`](../IKeymanKeyboardsInstalled2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language

[`IKeymanKeyboardFile::Install`](../IKeymanKeyboardFile/Install)
:   Installs a keyboard file and input method for default language
