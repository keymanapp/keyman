---
title: IKeymanLanguage Interface
---

## Introduction

The `IKeymanLanguage` interface lists a Windows input method entry.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanLanguage`**  

## Properties

[`BCP47Code`](BCP47Code) <span class="readonly">read only</span>
:   Returns a BCP 47 code for the Windows language, on operating systems
    that support it.

[`HKL`](HKL) <span class="readonly">read only</span>
:   Returns a Windows HKL for the input method.

[`Hotkey`](Hotkey)
:   Controls the current hotkey that will activate the keyboard layout
    with the selected language.

[`KeymanKeyboardLanguage`](KeymanKeyboardLanguage) <span class="readonly">read only</span>
:   Returns a
    [`IKeymanKeyboardLanguageInstalled`](../IKeymanKeyboardLanguageInstalled)
    reference for the Keyman keyboard language entry that this entry is
    associated with.

[`LangID`](LangID) <span class="readonly">read only</span>
:   Returns a Windows LANGID for the Windows language.

[`LayoutName`](LayoutName) <span class="readonly">read only</span>
:   Returns the display name of the input method.

[`LocaleName`](LocaleName) <span class="readonly">read only</span>
:   Returns the display name of the language.

[`ProfileGUID`](ProfileGUID) <span class="readonly">read only</span>
:   Returns the profile GUID associated with the Text Services Framework
    (TSF) Text Input Processor (TIP) installed for this keyboard, for
    this language.
