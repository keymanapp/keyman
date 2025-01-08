---
title: IKeymanPackage::ReadmeFile Property
---

## Introduction

The `IKeymanPackage::ReadmeFile` property returns an
[`IKeymanPackageContentFile`](../IKeymanPackageContentFile) reference
for the install Readme HTML file included within the package, or `null`
if no Readme file was included.

## Specification

``` clike
readonly IKeymanPackageContentFile* ReadmeFile
```
