---
title: IKeymanKeyboardOptions Interface
---

## Introduction

The `IKeymanKeyboardOptions` interface lists options associated with a
specific keyboard. The options are defined by the keyboard developer
through the [variable stores](/developer/language/guide/variable-stores)
feature.

These options are distinct from the
[`IKeymanOptions`](../IKeymanOptions) which control the operation of
Keyman Engine.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanKeyboardOptions`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanKeyboardOption`](../IKeymanKeyboardOption)
    reference for the option identified by `Index`.

<!-- -->

[`IndexOf()`](IndexOf)
:   Gets the index of an option by ID in the collection.
