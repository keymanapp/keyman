---
title: IKeymanKeyboardLanguageInstalled2::RegisterTip Method
---

## Introduction

The `IKeymanKeyboardLanguageInstalled2::RegisterTip` registers a Keyman
Text Services Framework Text Input Processor (TIP) input method for the
specified language. This method requires elevation.

## Specification

``` clike
void RegisterTip(long LangID);
```

## Parameters

`LangID`
:   The `LANGID` to register, which can be found with
    [`FindInstallationLangID`](FindInstallationLangID).

## See also

[`FindInstallationLangID()`](FindInstallationLangID)
:   Finds a Windows LANGID for this language's BCP 47 code, including
    optionally installing a transient language if required.

[`InstallTip()`](InstallTip)
:   Installs a registered language profile (persists over reboot) for
    the current user.
