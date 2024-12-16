---
title: IKeymanPackageContentFile::FullFilename Property
---

## Introduction

The `IKeymanPackageContentFile::FullFilename` property returns the
filename of the file including the path. This may be in a temporary
folder or in the Keyman package store. The file should not be modified
or deleted; when reading the file, use sharing semantics.

## Specification

``` clike
readonly string FullFilename
```
