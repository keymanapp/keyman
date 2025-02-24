---
title: IKeymanPackage::Filename Property
---

## Introduction

The `IKeymanPackage::Filename` property returns the fully qualified
filename of the package .kmp file, if the parent interface is
[`IKeymanPackageFile`](../IKeymanPackageFile), otherwise (interface is
[`IKeymanPackageInstalled`](../IKeymanPackageInstalled)), it returns the
fully qualified filename of the package kmp.inf file within the Keyman
package store.

## Specification

``` clike
readonly string Filename
```
