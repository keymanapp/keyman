---
title: IKeymanKeyboardLanguageInstalled2::IsRegistered Method
---

## Introduction

The `IKeymanKeyboardLanguageInstalled2::IsRegistered` returns `True` if
a Keyman Text Services Framework Text Input Processor (TIP) input method
is registered for the specified language.

## Specification

``` clike
bool IsRegisterTip();
```

## Returns

Returns `True` if the Keyman TIP is registered for this keyboard
language.

## See also

[`RegisterTip()`](RegisterTip)
:   Registers a language profile in Text Services Framework for this
    language (requires elevation).
