---
title: IKeymanKeyboardLanguageInstalled2::FindInstallationLangID Method
---

## Introduction

The `IKeymanKeyboardLanguageInstalled2::FindInstallationLangID` method
finds a LANGID for the language, which can then be passed into
[`IKeymanKeyboardLanguageInstalled2::RegisterTip`](RegisterTip) and
[`IKeymanKeyboardLanguageInstalled2::InstallTip`](InstallTip) to
complete registration.

If the BCP 47 tag does not have a corresponding system LANGID, then
Windows will allocate one of four transient LANGIDs to the tag. These
LANGIDs can differ between systems. In this situation, Windows installs
a temporary placeholder system keyboard which Keyman then replaces as it
completes the installation process.

## Specification

``` clike
bool FindInstallationLangID(long* LangID, string* TemporaryKeyboardID, bool *RegistrationRequired, tagKeymanInstallFlags Flags);
```

## Parameters

`LangID`
:   On success, this is filled with the corresponding `LANGID`.

`TemporaryKeyboardID`
:   On success, and if the `kifInstallTransientLanguage` flag is
    specified, this may contain a string which should be passed to
    `InstallTip` to be replaced with the Keyman keyboard. Internally,
    this string conforms to the Windows API `InstallLayoutorTip` format.

`RegistrationRequired`
:   If `True`, a call to `RegisterTip` will be required to complete
    installation of this language, as Keyman is not currently registered
    for this language with the Text Services Framework.

`Flags`
:   Can be `0` or `kifInstallTransientLanguage`. If
    `kifInstallTransientLanguage` is specified, then Keyman will install
    a transient language (Windows 8 or later) for BCP 47 tags that do
    not have a corresponding system LANGID.

## Returns

Returns `True` if a corresponding `LANGID` was found.

## See also

[`InstallTip()`](InstallTip)
:   Installs a registered language profile (persists over reboot) for
    the current user.

[`RegisterTip()`](RegisterTip)
:   Registers a language profile in Text Services Framework for this
    language (requires elevation).
