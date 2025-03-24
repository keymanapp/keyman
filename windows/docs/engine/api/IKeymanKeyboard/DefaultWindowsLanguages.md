---
title: IKeymanKeyboard::DefaultWindowsLanguages Property
---

## Introduction

The `IKeymanKeyboard::DefaultWindowsLanguages` property returns the
[`&windowslanguages`](/developer/language/reference/windowslanguages)
value from the keyboard file.

This is informational data and may not be the same as the currently
linked languages for the keyboard. This is a space separated list of
hexadecimal coded Windows LANGID values.

In the future, this will be deprecated in favour of
[`DefaultBCP47Languages`](DefaultBCP47Languages).

## Specification

``` clike
readonly long DefaultWindowsLanguages
```
