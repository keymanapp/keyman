---
title: IKeymanKeyboardLanguageInstalled2::InstallTip Method
---

## Introduction

The `IKeymanKeyboardLanguageInstalled2::InstallTip` installs a
registered Keyman Text Services Framework Text Input Processor (TIP)
input method for the specified language for the current user. The input
method is installed persistently over reboots.

## Specification

``` clike
void InstallTip(long LangID, string TemporaryKeyboardID);
```

## Parameters

`LangID`
:   The `LANGID` to install, which can be found with
    [`FindInstallationLangID`](FindInstallationLangID).

`TemporaryKeyboardID`
:   This value should be passed unmodified from the value returned from
    `FindInstallationLangID`. It allows Keyman to replace a temporary
    Windows keyboard with the desired Keyman keyboard, for transient
    LANGIDs.

## See also

[`FindInstallationLangID()`](FindInstallationLangID)
:   Finds a Windows LANGID for this language's BCP 47 code, including
    optionally installing a transient language if required.

[`RegisterTip()`](RegisterTip)
:   Registers a language profile in Text Services Framework for this
    language (requires elevation).
