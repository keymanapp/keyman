---
title: IKeymanKeyboardInstalled2::MCompileForBaseKeyboard Method
---

## Introduction

The `IKeymanKeyboardInstalled2::MCompileForBaseKeyboard` method recompiles
an installed keyboard layout against the specified Windows base
keyboard layout. The base layout is identified by its Windows Keyboard
Layout ID (KLID).

## Specification

``` clike
void MCompileForBaseKeyboard(integer KLID)
```

## Parameters

`KLID`
:   The Windows Keyboard Layout ID of the base keyboard layout to compile
    this keyboard against.
