---
title: IKeymanKeyboard::DefaultPrimaryLanguage Property
---

## Introduction

The `IKeymanKeyboard::DefaultPrimaryLanguage` property returns the
[`&language`](/developer/language/reference/language) value from the
keyboard file.

This is informational data and may not be the same as the currently
linked languages for the keyboard.

In the future, this will be deprecated in favour of
[`DefaultBCP47Languages`](DefaultBCP47Languages).

## Specification

``` clike
readonly long DefaultPrimaryLanguage
```
