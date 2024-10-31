---
title: IKeymanPackagsInstalled::IndexOf Method
---

## Introduction

The `IKeymanPackagsInstalled::IndexOf` method returns the zero-based
index of the package in the collection that has an ID matching the `ID`
parameter.

If no package matches, then the method returns `-1`.

## Specification

``` clike
long IndexOf(string ID)
```
