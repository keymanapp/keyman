---
title: IKeymanSystemInfo Interface
---

## Introduction

The `IKeymanSystemInfo` interface provides information about Keyman
Engine and the environment.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanSystemInfo`**  

## Properties

[`EngineInstallPath`](EngineInstallPath) <span class="readonly">read only</span>
:   Returns the fully qualified path where Keyman Engine is installed.

[`EngineVersion`](EngineVersion) <span class="readonly">read only</span>
:   Returns the version of Keyman Engine currently installed.

[`IsAdministrator`](IsAdministrator) <span class="readonly">read only</span>
:   Returns `True` if Keyman Engine has sufficient privileges to install
    keyboards and perform tasks with elevated privileges.

[`RebootRequired`](RebootRequired) <span class="readonly">read only</span>
:   Returns `True` if a previous action performed through the Keyman
    Engine API cannot be completed until the system is restarted.

## Methods

[`SetReboot()`](SetReboot)
:   Sets the reboot flag. This is typically used by Keyman Engine API
    internally but can also be set by a client process for their own
    reference.
