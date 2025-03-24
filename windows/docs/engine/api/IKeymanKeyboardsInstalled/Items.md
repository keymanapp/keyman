---
title: IKeymanKeyboardsInstalled::Items[Index] Property
---

## Introduction

The `IKeymanKeyboardsInstalled::Items[Index]` property returns a
reference to the installed keyboard identified by `Index`. If the
keyboard is not found, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanKeyboardInstalled* Items[variant Index]
```

## Parameters

Index
:   A zero-based integer index or the string
    [`IKeymanKeyboard::ID`](../IKeymanKeyboard/ID) of the keyboard.
