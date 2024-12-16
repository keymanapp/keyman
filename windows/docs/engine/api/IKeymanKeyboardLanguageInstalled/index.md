---
title: IKeymanKeyboardLanguageInstalled Interface
---

## Introduction

The `IKeymanKeyboardLanguageInstalled` interface lists a Windows
language associated with a keyboard in Keyman Engine for Windows API, or
a suggested language available for installation for the keyboard.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanKeyboardLanguage`](../IKeymanKeyboardLanguage)  
> >
> > > **`IKeymanKeyboardLanguageInstalled`**  
> > >
> > > > [`IKeymanKeyboardLanguageInstalled2`](../IKeymanKeyboardLanguageInstalled2)  

## Properties

[`OwnerKeyboard`](OwnerKeyboard) <span class="readonly">read only</span>
:   Returns the keyboard associated with the language.

[`ProfileGUID`](ProfileGUID) <span class="readonly">read only</span>
:   Returns the profile GUID associated with the Text Services Framework
    (TSF) Text Input Processor (TIP) installed for this keyboard, for
    this language, or `GUID_NULL` if the language is not currently
    installed.

[`IsInstalled`](IsInstalled) <span class="readonly">read only</span>
:   Returns `TRUE` if the language is currently installed for this
    keyboard.

## Methods

[`Install()`](Install)
:   Deprecated in 14.0. If not already installed, adds this keyboard
    layout to the Windows language, and if necessary installs the
    Windows language as well.

[`Uninstall()`](Uninstall)
:   Removes this keyboard layout from the Windows language, and if it is
    the last input method for the language, removes the language as
    well.
