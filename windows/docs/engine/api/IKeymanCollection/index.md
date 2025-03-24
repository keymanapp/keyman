---
title: IKeymanCollection Interface
---

## Introduction

The `IKeymanCollection` interface is the base interface for all
collections in Keyman Engine for Windows API. The `Items[]` property is
implemented in sub-interfaces.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanCollection`**  
> >
> > > [`IKeymanErrors`](../IKeymanErrors)  
> > > [`IKeymanHotkeys`](../IKeymanHotkeys)  
> > > [`IKeymanKeyboardLanguages`](../IKeymanKeyboardLanguages)  
> > >
> > > > [`IKeymanKeyboardLanguagesFile`](../IKeymanKeyboardLanguagesFile)  
> > > > [`IKeymanKeyboardLanguagesInstalled`](../IKeymanKeyboardLanguagesInstalled)  
> > > >
> > > > > [`IKeymanKeyboardLanguagesInstalled2`](../IKeymanKeyboardLanguagesInstalled2)  
> > >
> > > [`IKeymanKeyboardOptions`](../IKeymanKeyboardOptions)  
> > > [`IKeymanKeyboards`](../IKeymanKeyboards)  
> > >
> > > > [`IKeymanKeyboardsInstalled`](../IKeymanKeyboardsInstalled)  
> > > >
> > > > > [`IKeymanKeyboardsInstalled2`](../IKeymanKeyboardsInstalled2)  
> > > >
> > > > [`IKeymanPackageContentKeyboards`](../IKeymanPackageContentKeyboards)  
> > >
> > > [`IKeymanLanguages`](../IKeymanLanguages)  
> > > [`IKeymanOptions`](../IKeymanOptions)  
> > > [`IKeymanPackageContentFiles`](../IKeymanPackageContentFiles)  
> > > [`IKeymanPackageContentFonts`](../IKeymanPackageContentFonts)  
> > > [`IKeymanPackagesInstalled`](../IKeymanPackagesInstalled)  
> > >
> > > > [`IKeymanPackagesInstalled2`](../IKeymanPackagesInstalled2)  


## Properties

[`Count`](Count) <span class="readonly">read only</span>
:   Returns the number of items in the collection.

## Methods

[`_NewEnum()`](_NewEnum)
:   Instantiates an enumerator for the collection. This is required for
    COM enumeration of the collection.

<!-- -->

[`Refresh()`](Refresh)
:   Refreshes the collection from the current Keyman Engine
    configuration.
