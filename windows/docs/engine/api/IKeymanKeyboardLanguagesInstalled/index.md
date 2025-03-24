---
title: IKeymanKeyboardLanguagesInstalled Interface
---

## Introduction

The `IKeymanKeyboardLanguagesInstalled` interface lists the languages
associated with a keyboard in Keyman Engine for Windows API. This list
is the set of languages currently installed for use, or suggested for
the keyboard by the keyboard developer.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > [`IKeymanKeyboardLanguages`](../IKeymanKeyboardLanguages)  
> > >
> > > > **`IKeymanKeyboardLanguagesInstalled`**  
> > > >
> > > > > [`IKeymanKeyboardLanguagesInstalled2`](../IKeymanKeyboardLanguagesInstalled2)  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanKeyboardLanguageInstalled`](../IKeymanKeyboardLanguageInstalled)
    reference for the language identified by `Index`.

## Methods

[`Install()`](Install)
:   **Deprecated in 14.0.** Associates the keyboard layout with the
    referenced BCP 47 language code in Windows, and if necessary
    installs the language as well.

<!-- -->

[`InstallByLangID()`](InstallByLangID)
:   **Deprecated in 14.0.** Associates the keyboard layout with the
    referenced legacy LANGID language code in Windows, and if necessary
    installs the language as well.
