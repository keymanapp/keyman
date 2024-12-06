---
title: IKeymanKeyboardLanguagesFile::Items[Index] Property
---

## Introduction

The `IKeymanKeyboardLanguagesFile::Items[Index]` property returns a
reference to the installed language identified by `Index`. If the index
is out of range, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanKeyboardLanguage* Items[long Index]
```

## Parameters

Index
:   A zero-based index.
