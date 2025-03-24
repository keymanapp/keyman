---
title: IKeymanOptions::Items[Index] Property
---

## Introduction

The `IKeymanOptions::Items[Index]` property returns a
[`IKeymanOption`](../IKeymanOption) reference identified by `Index`. If
the index is out of range, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanOption* Items[variant Index]
```

## Parameters

Index
:   A zero-based integer index, or the string name of the option.
