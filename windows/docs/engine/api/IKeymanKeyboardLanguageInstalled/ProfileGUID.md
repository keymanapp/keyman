---
title: IKeymanKeyboardLanguageInstalled::ProfileGUID Property
---

## Introduction

The `IKeymanKeyboardLanguageInstalled::ProfileGUID` property returns the
profile GUID associated with the Text Services Framework (TSF) Text
Input Processor (TIP) installed for this keyboard, for this language. If
the language is not currently installed, then returns `GUID_NULL`.

## Specification

``` clike
readonly GUID ProfileGUID
```
