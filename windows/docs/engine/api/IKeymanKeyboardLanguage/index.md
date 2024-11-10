---
title: IKeymanKeyboardLanguage Interface
---

## Introduction

The `IKeymanKeyboardLanguage` interface lists a language associated with
a keyboard in Keyman Engine for Windows API.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanKeyboardLanguage`**  
> >
> > > [`IKeymanKeyboardLanguageInstalled`](../IKeymanKeyboardLanguageInstalled)  
> > >
> > > > [`IKeymanKeyboardLanguageInstalled2`](../IKeymanKeyboardLanguageInstalled2)  

## Properties

[`BCP47Code`](BCP47Code) <span class="readonly">read only</span>
:   Returns a BCP 47 code for the Windows language.

<!-- -->

[`LangID`](LangID) <span class="readonly">read only</span>
:   Returns a Windows LANGID for the Windows language.

<!-- -->

[`OwnerKeyboard`](OwnerKeyboard) <span class="readonly">read only</span>
:   Returns the keyboard associated with the language.

<!-- -->

[`Name`](Name) <span class="readonly">read only</span>
:   Returns the language name for the language.
