---
title: IKeymanKeyboardsInstalled Interface
---

## Introduction

The `IKeymanKeyboardsInstalled` interface lists the installed keyboards
in Keyman Engine for Windows API.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > [`IKeymanKeyboards`](../IKeymanKeyboards)  
> > >
> > > > **`IKeymanKeyboardsInstalled`**  
> > > >
> > > > > [`IKeymanKeyboardsInstalled2`](../IKeymanKeyboardsInstalled2)  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanKeyboardInstalled`](../IKeymanKeyboardInstalled)
    reference for the keyboard identified by `Index`.

## Methods

[`GetKeyboardFromFile()`](GetKeyboardFromFile)
:   Returns an [`IKeymanKeyboardFile`](../IKeymanKeyboardFile) reference
    for a keyboard file on disk.

[`Install()`](Install)
:   Installs the referenced keyboard file into Keyman Engine for
    Windows.

[`Apply()`](Apply)
:   Applies changes made to the keyboards in the collection to Keyman
    Engine for Windows.
