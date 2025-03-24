---
title: IKeymanLanguages::Items[Index] Property
---

## Introduction

The `IKeymanLanguages::Items[Index]` property returns a
[`IKeymanLanguage`](../IKeymanLanguage) reference to the installed
language identified by `Index`. If the index is out of range, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanLanguage* Items[long Index]
```

## Parameters

Index
:   A zero-based index.
