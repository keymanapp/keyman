---
title: IKeymanHotkey Interface
---

## Introduction

The `IKeymanHotkey` interface describes a hotkey associated with an
action or a keyboard.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanHotkey`**  


## Properties

[`Modifiers`](Modifiers)
:   Returns a set of modifier keys used by the hotkey.

<!-- -->

[`RawValue`](RawValue)
:   Returns the raw hotkey value, a combination of `Modifiers` and
    `VirtualKey`.

<!-- -->

[`Target`](Target) <span class="readonly">read only</span>
:   Returns the action that will be taken when the hotkey is pressed by
    the user.

<!-- -->

[`VirtualKey`](VirtualKey)
:   Returns the Windows virtual key code of the hotkey.

## Methods

[`Clear()`](Clear)
:   Clears the hotkey value. It does not remove the hotkey from the
    collection.

<!-- -->

[`IsEmpty()`](IsEmpty)
:   Returns `True` if the hotkey is empty.
