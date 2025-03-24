---
title: IKeymanPackage::Version Property
---

## Introduction

The `IKeymanPackage::Version` property returns the version string from
the package file. This represents the current version of the package
file, and is in the form of a dotted numeric string.

The `Version` property is not directly related to the
[`IKeymanKeyboard::Version`](../IKeymanKeyboard/Version) property of the
keyboards within the package.

## Specification

``` clike
readonly string Version
```
