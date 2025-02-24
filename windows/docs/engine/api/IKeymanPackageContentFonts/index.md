---
title: IKeymanPackageContentFonts Interface
---

## Introduction

The `IKeymanPackageContentFonts` interface lists the fonts included in
the package.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanPackageContentFonts`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFont`](../IKeymanPackageContentFont)
    reference for the font identified by `Index`.

[`IndexOf()`](IndexOf)
:   Gets the index of a font by filename in the collection.
