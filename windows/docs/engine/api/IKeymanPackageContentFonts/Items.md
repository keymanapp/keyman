---
title: IKeymanPackageContentFonts::Items[Index] Property
---

## Introduction

The `IKeymanPackageContentFonts::Items[Index]` property returns a
[`IKeymanPackageContentFont`](../IKeymanPackageContentFont) reference to
the font identified by `Index` in the package. If the font is not in the
package, or the index is out of range, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.

## Specification

``` clike
readonly IKeymanPackageContentFont* Items[variant Index]
```

## Parameters

Index
:   A zero-based integer index or the string filename of the font.
