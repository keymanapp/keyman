---
title: IKeymanKeyboardFile Interface
---

## Introduction

The `IKeymanKeyboardFile` interface describes a keyboard file. The file
will not have been installed but can be any keyboard file on disk.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanKeyboard`](../IKeymanKeyboard)  
> >
> > > **`IKeymanKeyboardFile`**  
> > >
> > > > [`IKeymanKeyboardFile2`](../IKeymanKeyboardFile2)  

## Properties

[`Languages`](Languages)
:   Lists the languages associated with the keyboard, as retrieved from
    package metadata.

## Methods

[`Install()`](Install)
:   Installs the keyboard file into Keyman Engine for Windows.
