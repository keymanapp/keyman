---
title: IKeyman Interface
---

## Introduction

The `IKeyman` interface is the top-level interface for the Keyman Engine
for Windows API. The `IKeyman` interface can be instantiated with the
`CoKeyman` coclass; it will also be returned from a call to
`CreateObject("kmcomapi.Keyman")`.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeyman`**  

## Properties

[`AutoApply`](AutoApply)
:   Determines whether settings changes are applied to Keyman Engine
    automatically as they are made.

[`Control`](Control) <span class="readonly">read only</span>
:   Returns an [`IKeymanControl`](../IKeymanControl) interface that
    provides methods for controlling the Keyman process and user
    interface.

[`Errors`](Errors) <span class="readonly">read only</span>
:   Returns an [`IKeymanErrors`](../IKeymanErrors) interface that lists
    errors that the API incurred when undertaking various processes.

[`Hotkeys`](Hotkeys) <span class="readonly">read only</span>
:   Returns an [`IKeymanHotkeys`](../IKeymanHotkeys) interface that
    lists all of the configured interface hotkeys.

[`Keyboards`](Keyboards) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanKeyboardsInstalled`](../IKeymanKeyboardsInstalled)
    interface that lists all of the currently installed Keyman
    keyboards.

[`Languages`](Languages) <span class="readonly">read only</span>
:   Returns an [`IKeymanLanguages`](../IKeymanLanguages) interface that
    lists all of the currently avilable Windows languages and input
    methods.

[`Options`](Options) <span class="readonly">read only</span>
:   Returns an [`IKeymanOptions`](../IKeymanOptions) interface that
    lists all of the configuration settings and values for Keyman
    Engine.

[`Packages`](Packages) <span class="readonly">read only</span>
:   Returns an [`IKeymanPackagesInstalled`](../IKeymanPackagesInstalled)
    interface that lists all of the currently installed Keyman packages.

[`SystemInfo`](SystemInfo) <span class="readonly">read only</span>
:   Returns an [`IKeymanSystemInfo`](../IKeymanSystemInfo) interface
    that describes the current system configuration.

## Methods

[`Apply()`](Apply)
:   Apply all settings changes to Keyman Engine.

[`Refresh()`](Refresh)
:   Refresh the snapshot of keyboards and settings which is captured
    when the object is instantiated.

## Examples

### VBScript

This example can be saved with a .vbs extension to run through Windows
Scripting. This is similar to the uninstall script that Keyman Desktop
generates for packages which have a Start Menu Uninstall entry.

``` basic
dim kmcom, package
Set kmcom = CreateObject("keymanapi.Keyman")
n = kmcom.Packages.IndexOf("SamplePackage")
if n >= 0 then
  Set package = kmcom.Packages(n)
  if msgbox("Uninstall package "+package.Description+"?", vbOKCancel, "Keyman Desktop") = vbOK then
    package.Uninstall(True)
  end if
else
  msgbox "The package SamplePackage could not be found."
end if
```
