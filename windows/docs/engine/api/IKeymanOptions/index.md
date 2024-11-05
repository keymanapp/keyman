---
title: IKeymanOptions Interface
---

## Introduction

The `IKeymanOptions` interface lists options available in Keyman Engine.
These options are distinct from the
[`IKeymanKeyboardOptions`](../IKeymanKeyboardOptions) which control how
specific keyboard layouts operate.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanOptions`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanOption`](../IKeymanOption) reference by `Index`.

## Methods

[`Apply()`](Apply)
:   Applies changes to options to Keyman Engine.

[`IndexOf()`](IndexOf)
:   Gets the index of an option by ID in the collection.
