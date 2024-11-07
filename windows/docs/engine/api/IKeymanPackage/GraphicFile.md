---
title: IKeymanPackage::GraphicFile Property
---

## Introduction

The `IKeymanPackage::GraphicFile` property returns an
[`IKeymanPackageContentFile`](../IKeymanPackageContentFile) reference
for the install screen graphic included within the package, or `null` if
no install screen graphic was included.

## Specification

``` clike
readonly IKeymanPackageContentFile* GraphicFile
```
