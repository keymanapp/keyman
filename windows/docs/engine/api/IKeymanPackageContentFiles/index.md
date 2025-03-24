---
title: IKeymanPackageContentFiles Interface
---

## Introduction

The `IKeymanPackageContentFiles` interface lists the files included in
the package.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanPackageContentFiles`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFile`](../IKeymanPackageContentFile)
    reference for the file identified by `Index`.

<!-- -->

[`IndexOf()`](IndexOf)
:   Gets the index of a file by filename in the collection.
