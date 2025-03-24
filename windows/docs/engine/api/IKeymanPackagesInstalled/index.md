---
title: IKeymanPackagesInstalled Interface
---

## Introduction

The `IKeymanPackagesInstalled` interface lists the packages installed in
Keyman Engine.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanPackagesInstalled`**  
> > >
> > > > [`IKeymanPackagesInstalled2`](../IKeymanPackagesInstalled2)  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanPackageInstalled`](../IKeymanPackageInstalled)
    reference for the package identified by `Index`.

## Methods

[`GetPackageFromFile()`](GetPackageFromFile)
:   Returns an [`IKeymanPackageFile`](../IKeymanPackageFile) reference
    for a package file.

[`IndexOf()`](IndexOf)
:   Gets the index of a package by ID in the collection.

[`Install()`](Install)
:   Installs a package file into Keyman Engine.
