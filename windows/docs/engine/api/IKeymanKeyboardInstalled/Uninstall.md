---
title: IKeymanKeyboardInstalled::Uninstall Method
---

## Introduction

The `IKeymanKeyboardInstalled::Uninstall` method uninstalls the keyboard
file (.kmx) and related artefacts from Keyman Engine for Windows. This
includes deleting the files from the Keyman Engine keyboard store,
removing registry settings, and removing Windows input methods.

This method removes the keyboard layout from all languages it is
associated with.

Changes associated with calling this method are applied immediately;
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
does not need to be called. However, changes to the Windows input
methods are applied asynchronously.

This method requires elevated privileges.

## Specification

``` clike
void Uninstall(bool void)
```
