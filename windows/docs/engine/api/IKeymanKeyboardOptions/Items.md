---
title: IKeymanKeyboardOptions::Items[Index] Property
---

## Introduction

The `IKeymanKeyboardOptions::Items[Index]` property returns a reference
to the keyboard option by `Index`. If the option is not found, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanKeyboardOption* Items[variant Index]
```

## Parameters

Index
:   A zero-based index, or a string referencing the option by
    [`IKeymanKeyboardOption::Name`](../IKeymanKeyboardOption/Name).
