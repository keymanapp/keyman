---
title: IKeymanKeyboardLanguagesInstalled2::Add Method
---

## Introduction

The `IKeymanKeyboardLanguagesInstalled2::Add` adds an entry to the
in-memory list in preparation for registration and installation with the
functions in the
[`IKeymanKeyboardLanguageInstalled2`](../IKeymanKeyboardLanguageInstalled2)
interface.

## Specification

``` clike
IKeymanKeyboardLanguagesInstalled Add(string Bcp47Tag)
```

## Parameters

`Bcp47Tag`
:   The BCP 47 tag to associate the keyboard with. Windows will
    normalize these codes so after installation you may find the layout
    available under a normalized code and not the one you expected.
