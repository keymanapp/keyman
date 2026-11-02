---
title: IKeymanKeyboardsInstalled3::Install3 Method
---

## Introduction

The `IKeymanKeyboardsInstalled3::Install3` method installs a keyboard
file (.kmx) into Keyman Engine for Windows. This includes copying the
file into the Keyman Engine keyboard store, adding registry settings,
and optionally installing the Windows input method. During installation,
the keyboard layout is compiled against the specified base keyboard. As
installing Windows input methods is an asynchronous process, this method
may not be complete when it returns.

If the input method is installed, this method adds the keyboard to the
default language as specified in the
[`IKeymanKeyboard::DefaultBCP47Languages`](../IKeymanKeyboard/DefaultBCP47Languages)
property.

This method requires elevated privileges.

## Specification

``` clike
void Install3(string Filename, bool Force, int BaseKeyboardID)
```

## Parameters

`Filename`
:   The fully-qualified path to the .kmx file to be installed. This file
    will be copied to the Keyman keyboard store.

`Force`
:   If `True`, overwrites an existing keyboard entry.

`BaseKeyboardID`
:   The ID of the base keyboard to compile the keyboard layout against.

## See also

[`IKeymanKeyboardsInstalled2::Install2`](../IKeymanKeyboardsInstalled2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language

[`IKeymanKeyboardFile3::Install3`](../IKeymanKeyboardFile3/Install3)
:   Installs a keyboard file and compiles its layout against a specified
    base keyboard
