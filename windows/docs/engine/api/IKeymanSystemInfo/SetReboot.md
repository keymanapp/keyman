---
title: IKeymanSystemInfo::SetReboot Method
---

## Introduction

The `IKeymanSystemInfo::SetReboot` method sets the reboot flag
[`RebootRequired`](RebootRequired). This is typically used by Keyman
Engine API internally but can also be set by a client process for their
own reference.

## Specification

``` clike
void SetReboot(void)
```
