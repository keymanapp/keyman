---
title: IKeymanKeyboardLanguagesInstalled::Install Method
---

## Introduction

**Deprecated:** this function is deprecated as of Keyman 14.0. Instead,
use the functions in
[`IKeymanKeyboardLanguageInstalled2`](../IKeymanKeyboardLanguageInstalled2).

The `IKeymanKeyboardLanguagesInstalled::Install` adds the keyboard
layout to the referenced BCP 47 language code in Windows. If the
language code is not yet installed, this method installs it as well.

This method is asynchronous.

This method requires elevated privileges.

## Specification

``` clike
void Install(string Bcp47Code)
```

## Parameters

`Bcp47Code`
:   The BCP 47 code to associate the keyboard with. Windows will
    normalize these codes so after installation you may find the layout
    available under a normalized code and not the one you expected.
