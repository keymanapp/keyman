---
title: IKeymanControl::OpenConfiguration() Method
---

## Introduction

The `IKeymanControl::OpenConfiguration()` method starts Keyman
Configuration (kmshell.exe). It is better to call this method than to
launch kmshell.exe directly as this method handles paths and parameter
changes across versions of Keyman Engine.

## Specification

``` clike
void OpenConfiguration(void)
```
