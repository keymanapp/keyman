---
title: IKeymanKeyboard::Version Property
---

## Introduction

The `IKeymanKeyboard::Version` property returns the
[`&keyboardversion`](/developer/language/reference/keyboardversion)
string from the keyboard file. This represents the current version of
the keyboard file, and is in the form of a dotted numeric string.

The `Version` property is not related to the
[`&version`](/developer/language/reference/version) store, which
determines the minimum Keyman Engine version which a keyboard can be run
under.

## Specification

``` clike
readonly string Version
```
