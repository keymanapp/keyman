---
title: IKeymanKeyboard::Version GetCharsUsed Method
---

## Introduction

The `IKeymanKeyboard::GetCharsUsed` method returns the character
repertoire of the keyboard layout, that is, the Unicode characters that
the keyboard generates and references. This can be helpful for
determining support for a given keyboard, e.g. related fonts, and is
built up by scanning all the keyboard output and context strings in the
keyboard layout.

## Specification

``` clike
string GetCharsUsed(void)
```
