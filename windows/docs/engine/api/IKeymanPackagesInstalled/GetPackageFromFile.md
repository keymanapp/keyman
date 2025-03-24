---
title: IKeymanPackagesInstalled::GetPackageFromFile Method
---

## Introduction

The `IKeymanPackagesInstalled::GetPackageFromFile` method loads the
package file referred to by Filename and returns details about the file.
It does not install the package for use.

## Specification

``` clike
IKeymanPackageFile* GetPackageFromFile(string Filename)
```

## Parameters

Filename
:   The fully-qualified path to the .kmp file to be loaded.
