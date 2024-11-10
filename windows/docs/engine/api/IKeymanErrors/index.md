---
title: IKeymanErrors Interface
---

## Introduction

The `IKeymanErrors` interface lists errors encountered by Keyman Engine
in a previous command such as installing a package.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanErrors`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanError`](../IKeymanError) reference at `Index`.

## Methods

[`Clear()`](Clear)
:   Clears the collection.
