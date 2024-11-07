---
title: IKeymanControl::StartKeyman() Method
---

## Introduction

The `IKeymanControl::StartKeyman()` method starts Keyman Engine
(keyman.exe). It is better to call this method than to launch keyman.exe
directly, as this method handles paths and parameter changes across
versions of Keyman Engine.

## Specification

``` clike
void StartKeyman(void)
```
