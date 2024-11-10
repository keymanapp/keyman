---
title: IKeymanKeyboardInstalled::VisualKeyboard Property
---

## Introduction

The `IKeymanKeyboardInstalled::VisualKeyboard` property returns an
[`IKeymanVisualKeyboard`](../IKeymanVisualKeyboard) reference for the
visual keyboard associated with this keyboard. If no visual keyboard is
installed for this keyboard, then `VisualKeyboard` returns `null`.

A visual keyboard can be installed with
[`InstallVisualKeyboard()`](InstallVisualKeyboard).

## Specification

``` clike
readonly IKeymanVisualKeyboard* VisualKeyboard
```
