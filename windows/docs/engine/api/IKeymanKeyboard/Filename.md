---
title: IKeymanKeyboard::Filename Property
---

## Introduction

The `IKeymanKeyboard::Filename` property returns the fully qualified
filename of the keyboard .kmx file. If the parent interface is
[`IKeymanKeyboardFile`](../IKeymanKeyboardFile), then the filename is
the same as the file used to instantiate the interface; otherwise, the
file will be in the Keyman keyboard store, and should not be moved out
of the store except by calling the
[`IKeymanKeyboardInstalled::Uninstall`](../IKeymanKeyboardInstalled/Uninstall)
method.

## Specification

``` clike
readonly string Filename
```
