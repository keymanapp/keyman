---
title: IKeymanKeyboard::Encodings Property
---

## Introduction

The `IKeymanKeyboard::Encodings` property returns the supported
encodings for the keyboard. Most keyboards should be `keUnicode`, but
some legacy keyboards may return `keANSI`. It is possible for a keyboard
to support both `keUnicode` and `keANSI` but these keyboards are rare
and not recommended for general use.

## Specification

``` clike
readonly KeymanKeyboardEncodings Encodings
```

## Encodings

| Value | Name      |
|-------|-----------|
| 1     | keANSI    |
| 2     | keUnicode |
