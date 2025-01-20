---
title: IKeymanErrors::Items[Index] Property
---

## Introduction

The `IKeymanErrors::Items[Index]` property returns a
[`IKeymanError`](../IKeymanError) reference identified by `Index`. If
the index is out of range, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanError* Items[long Index]
```

## Parameters

Index
:   A zero-based integer index .
