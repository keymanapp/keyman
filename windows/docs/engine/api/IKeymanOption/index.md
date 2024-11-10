---
title: IKeymanOption Interface
---

## Introduction

The `IKeymanOption` interface describes a user-configurable option in
Keyman Engine.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanOption`**  

## Properties

[`DefaultValue`](DefaultValue) <span class="readonly">read only</span>
:   Returns the default value for the option.

[`Enabled`](Enabled) <span class="readonly">read only</span>
:   Returns `True` if the option is currently enabled for the user.

[`Group`](Group) <span class="readonly">read only</span>
:   Returns a grouping identifier for the option.

[`ID`](ID) <span class="readonly">read only</span>
:   Returns an identifier for the option. Identifiers are mapped to
    user-readable strings in the user interface translation files.

[`OptionType`](OptionType) <span class="readonly">read only</span>
:   Returns the value data type supported by the option, one of
    `kotBool`, `kotString` or `kotLong`.

[`Value`](Value)
:   Reads or sets the current value for the option.
