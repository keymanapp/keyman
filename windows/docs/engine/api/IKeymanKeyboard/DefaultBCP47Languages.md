---
title: IKeymanKeyboard::DefaultBCP47Languages Property
---

## Introduction

The `IKeymanKeyboard::DefaultBCP47Languages` property returns the
[`&ethnologuecode`](/developer/language/reference/ethnologuecode) string
from the keyboard file. The first language code in the string is the
primary language.

This is informational data and may not be the same as the currently
linked languages for the keyboard.

## Specification

``` clike
readonly string DefaultBCP47Languages
```
