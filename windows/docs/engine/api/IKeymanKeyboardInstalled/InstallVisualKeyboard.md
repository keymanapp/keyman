---
title: IKeymanKeyboardInstalled::InstallVisualKeyboard Method
---

## Introduction

The `IKeymanKeyboardInstalled::InstallVisualKeyboard` method installs a
visual keyboard file (.kvk) and associates it with the keyboard. The
visual keyboard file is copied into the Keyman keyboard store. If a
visual keyboard is already installed for the keyboard, then the new
visual keyboard replaces the old one.

Typically, a keyboard and visual keyboard will be installed together as
part of a package, but this method can be helpful for testing and
development of keyboards.

Changes associated with calling this method are applied immediately;
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
does not need to be called.

A visual keyboard can be uninstalled with
[`IKeymanVisualKeyboard::Uninstall`](../IKeymanVisualKeyboard/Uninstall).

This method requires elevated privileges.

## Specification

``` clike
void InstallVisualKeyboard(string Filename)
```
