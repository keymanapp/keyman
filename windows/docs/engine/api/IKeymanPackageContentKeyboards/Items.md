---
title: IKeymanPackageContentKeyboards::Items[Index] Property
---

## Introduction

The `IKeymanPackageContentKeyboards::Items[Index]` property returns a
reference to the keyboard identified by `Index` in the package. If the
keyboard does not exist, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanKeyboard* Items[variant Index]
```

## Parameters

Index
:   A zero-based integer index or the string
    [`IKeymanKeyboard::ID`](../IKeymanKeyboard/ID) of the keyboard.
