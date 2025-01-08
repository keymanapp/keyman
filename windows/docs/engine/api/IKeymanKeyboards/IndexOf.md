---
title: IKeymanKeyboards::IndexOf Method
---

## Introduction

The `IKeymanKeyboards::IndexOf` method returns the zero-based index of
the keyboard in the array that has an ID matching the `ID` parameter.
The ID is formed from the base name of the keyboard, i.e. excluding path
and extension, and is case insensitive.

If no keyboard matches, then the method returns `-1`.

## Specification

``` clike
long IndexOf(string ID)
```
