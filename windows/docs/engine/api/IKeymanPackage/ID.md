---
title: IKeymanPackage::ID Property
---

## Introduction

The `IKeymanPackage::ID` property returns the base name of the package
.kmp file, sans extension. The full filename, including path, of the
package is available in the [`Filename`](Filename) property.

No two packages can share the same identifier in a Keyman Engine
installation. The identifier is case insensitive and should consist of
letters, digits and underscore, although some legacy identifiers may
contain other characters as well. The initial letter of an identifier
should be a letter.

## Specification

``` clike
readonly string ID
```
