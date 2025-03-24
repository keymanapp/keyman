---
title: IKeymanError Interface
---

## Introduction

The `IKeymanError` interface provides information about an error that
Keyman Engine encountered.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanError`**  


## Properties

[`ErrorCode`](ErrorCode) <span class="readonly">read only</span>
:   Returns an integer code for the error.

<!-- -->

[`Description`](Description) <span class="readonly">read only</span>
:   Returns a human-readable description of the error.

<!-- -->

[`Severity`](Severity) <span class="readonly">read only</span>
:   Returns a `KeymanErrorSeverity` value for the error.
