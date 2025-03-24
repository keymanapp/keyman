---
title: IKeymanKeyboardsInstalled::Install Method
---

## Introduction

The `IKeymanKeyboardsInstalled::Install` installs a keyboard file (.kmx)
into Keyman Engine for Windows. This includes copying the file into the
Keyman Engine keyboard store, adding registry settings, and installing
the Windows input method. As installing Windows input methods is an
asynchronous process, this method may not be complete when it returns.

This method adds the keyboard to the default language as specified in
the
[`IKeymanKeyboard::DefaultBCP47Languages`](../IKeymanKeyboard/DefaultBCP47Languages)
property.

## Specification

``` clike
void Install(string Filename, bool Force)
```

## Parameters

`Filename`
:   The fully-qualified path to the .kmx file to be installed. This file
    will be copied to the Keyman keyboard store.

`Force`
:   If `True`, overwrites an existing keyboard entry.

## See also

[`IKeymanKeyboardsInstalled::Install2`](../IKeymanKeyboardsInstalled2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language

[`IKeymanKeyboardFile::Install`](../IKeymanKeyboardFile/Install)
:   Installs a keyboard file and input method for default language

[`IKeymanKeyboardFile2::Install2`](../IKeymanKeyboardFile2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language
