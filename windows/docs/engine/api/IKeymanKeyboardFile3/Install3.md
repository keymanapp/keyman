---
title: IKeymanKeyboardFile3::Install3 Method
---

## Introduction

The `IKeymanKeyboardFile3::Install3` method installs the keyboard file
(.kmx) into Keyman Engine for Windows. During installation, the keyboard
layout is compiled against the specified base keyboard. As installing
Windows input methods is an asynchronous process, this method may not be
complete when it returns.

This method requires elevated privileges.

## Specification

``` clike
void Install3(bool Force, int BaseKeyboardID)
```

## Parameters

`Force`
:   If `True`, overwrites an existing keyboard entry.

`BaseKeyboardID`
:   The ID of the base keyboard to compile the keyboard layout against.

## See also

[`IKeymanKeyboardsInstalled::Install`](../IKeymanKeyboardsInstalled/Install)
:   Installs a keyboard file and input method for default language

[`IKeymanKeyboardsInstalled::Install2`](../IKeymanKeyboardsInstalled2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language

[`IKeymanKeyboardFile::Install`](../IKeymanKeyboardFile/Install)
:   Installs a keyboard file and input method for default language

[`IKeymanKeyboardFile2::Install2`](../IKeymanKeyboardFile2/Install2)
:   Installs a keyboard file, optionally with input method for default
    language
