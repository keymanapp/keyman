---
title: IKeymanKeyboardLanguageInstalled::IsInstalled Property
---

## Introduction

The `IKeymanKeyboardLanguageInstalled::IsInstalled` property returns
`TRUE` if the keyboard is currently associated with this language in
Windows. If the language is not currently installed, then it is a
suggested language for the keyboard, per the package metadata.

## Specification

``` clike
readonly BOOL IsInstalled
```
