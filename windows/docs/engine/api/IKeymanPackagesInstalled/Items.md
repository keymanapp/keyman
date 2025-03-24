---
title: IKeymanPackagesInstalled::Items[Index] Property
---

## Introduction

The `IKeymanPackagesInstalled::Items[Index]` property returns a
reference to the installed package identified by `Index`. If the package
is not found, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanPackageInstalled* Items[variant Index]
```

## Parameters

Index
:   A zero-based integer index or the string
    [`IKeymanPackage::ID`](../IKeymanPackage/ID) of the package.
