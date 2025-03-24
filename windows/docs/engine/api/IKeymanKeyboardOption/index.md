---
title: IKeymanKeyboardOption Interface
---

## Introduction

The `IKeymanKeyboardOption` interface lists an option associated with a
specific keyboard. The options are defined by the keyboard developer
through the [variable stores](/developer/language/guide/variable-stores)
feature.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanKeyboardOption`**  

## Properties

[`Name`](Name) <span class="readonly">read only</span>
:   Returns the name of the option as defined by the keyboard developer.

<!-- -->

[`Value`](Value)
:   Controls the current value of the option. Valid values depend on the
    keyboard layout.
