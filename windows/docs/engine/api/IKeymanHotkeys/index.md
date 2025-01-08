---
title: IKeymanHotkeys Interface
---

## Introduction

The `IKeymanHotkeys` interface lists hotkeys configured for Keyman
Engine.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanCollection`](../IKeymanCollection)  
> >
> > > **`IKeymanHotkeys`**  

## Properties

[`Items[Index]`](Items) <span class="readonly">read only</span>
:   Returns an [`IKeymanHotkey`](../IKeymanHotkey) reference for the
    hotkey identified by `Index`.

## Methods

[`Apply()`](Apply)
:   Applies changes to the hotkeys in the collection to Keyman Engine.


[`Reset()`](Reset)
:   Resets all hotkeys in the collection to empty.
