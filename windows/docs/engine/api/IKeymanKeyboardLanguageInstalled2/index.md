---
title: IKeymanKeyboardLanguageInstalled2 Interface
---

## Introduction

The `IKeymanKeyboardLanguageInstalled2` interface adds extension
functions for installing languages. These functions should be used in
preference to the now-deprecated
[`IKeymanKeyboardLanguagesInstalled::Install`](../IKeymanKeyboardLanguagesInstalled/Install)
and
[`IKeymanKeyboardLanguagesInstalled::InstallByLangID`](../IKeymanKeyboardLanguagesInstalled/InstallByLangID)
methods.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanKeyboardLanguage`](../IKeymanKeyboardLanguage)  
> >
> > > [`IKeymanKeyboardLanguageInstalled`](../IKeymanKeyboardLanguageInstalled)  
> > >
> > > > **`IKeymanKeyboardLanguageInstalled2`**  

## Methods

[`FindInstallationLangID()`](FindInstallationLangID)
:   Finds a Windows LANGID for this language's BCP 47 code, including
    optionally installing a transient language if required.

[`InstallTip()`](InstallTip)
:   Installs a registered language profile (persists over reboot) for
    the current user.

[`IsRegistered()`](IsRegistered)
:   Returns true if there is an associated registered language profile.

[`RegisterTip()`](RegisterTip)
:   Registers a language profile in Text Services Framework for this
    language (requires elevation).

## History

Added in Keyman Engine for Windows 14.0.
