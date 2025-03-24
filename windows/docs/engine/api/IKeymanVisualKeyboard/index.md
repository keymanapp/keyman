---
title: IKeymanVisualKeyboard Interface
---

## Introduction

The `IKeymanVisualKeyboard` interface references the visual keyboard
associated with an installed
[`IKeymanKeyboardInstalled`](../IKeymanKeyboardInstalled) keyboard
layout.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanVisualKeyboard`**  

## Properties

[`Filename`](Filename) <span class="readonly">read only</span>
:   Returns the fully qualified filename of the visual keyboard .kvk
    file.

## Methods

[`Uninstall()`](Uninstall)
:   Removes this visual keyboard from the keyboard and the Keyman
    keyboard store.
