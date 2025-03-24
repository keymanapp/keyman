---
title: IKeymanKeyboardLanguagesInstalled::InstallByLangID Method
---

## Introduction

**Deprecated:** this function is deprecated as of Keyman 14.0. Instead,
use the functions in
[`IKeymanKeyboardLanguageInstalled2`](../IKeymanKeyboardLanguageInstalled2).

The `IKeymanKeyboardLanguagesInstalled::InstallByLangID` adds the
keyboard layout to the referenced LangID language code in Windows. If
the language code is not yet installed, this method installs it as well.

This method is asynchronous.

This method requires elevated privileges.

## Specification

``` clike
void InstallByLangID(long LangID)
```

## Parameters

`LangID`
:   The language ID code to associate the keyboard with.
