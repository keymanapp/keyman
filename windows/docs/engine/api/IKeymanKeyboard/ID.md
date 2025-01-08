---
title: IKeymanKeyboard::ID Property
---

## Introduction

The `IKeymanKeyboard::ID` property returns the base name of the keyboard
.kmx file, sans extension. The full filename, including path, of the
keyboard is available in the [`Filename`](Filename) property.

No two keyboards can share the same identifier in a Keyman Engine
installation. The identifier is case insensitive and should consist of
letters, digits and underscores, although some legacy identifiers may
contain other characters as well. The initial character of an identifier
should be a letter.

## Specification

``` clike
readonly string ID
```
