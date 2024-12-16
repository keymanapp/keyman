---
title: IKeymanPackageContentFiles::Items[Index] Property
---

## Introduction

The `IKeymanPackageContentFiles::Items[Index]` property returns a
[`IKeymanPackageContentFile`](../IKeymanPackageContentFile) reference to
the file identified by `Index` in the package. If the file is not in the
package, or the index is out of range, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanPackageContentFile* Items[variant Index]
```

## Parameters

Index
:   A zero-based integer index or the string filename of the file. The
    filename must include the extension, but paths are stripped off and
    ignored.
