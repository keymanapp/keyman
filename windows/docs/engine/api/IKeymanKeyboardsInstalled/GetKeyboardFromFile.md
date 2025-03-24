---
title: IKeymanKeyboardsInstalled::GetKeyboardFromFile Method
---

## Introduction

The `IKeymanKeyboardsInstalled::GetKeyboardFromFile` method loads the
keyboard file referred to by Filename and returns details about the
file. It does not install the keyboard for use.

## Specification

``` clike
IKeymanKeyboardFile* GetKeyboardFromFile(string Filename)
```

## Parameters

Filename
:   The fully-qualified path to the .kmx file to be loaded.
