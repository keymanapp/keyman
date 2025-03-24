---
title: IKeymanLanguages Interface
---

## Introduction

The `IKeymanLanguages` interface lists all the Windows input methods
currently loaded on the system. As Keyman keyboards are installed as
Windows input methods, this interface will list Keyman input methods as
well as Windows system-supplied input methods and other third party
input methods that present as Windows input methods.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanLanguages`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanLanguage`](../IKeymanLanguage) reference for the
    windows language identified by `Index`.
