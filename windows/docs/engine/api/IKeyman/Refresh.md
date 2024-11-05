---
title: IKeyman::Refresh Method
---

## Introduction

When the `CoKeyman` object is instantiated, the Keyman API takes a
snapshot of the installed keyboards and various settings in Keyman
Engine. The `IKeyman::Refresh` method refreshes this snapshot with the
current Keyman and system configuration. After `Refresh` is called, any
references to interfaces held by the API consumer must be discarded, as
the API implementation releases them and creates new instances, even if
the object in question has had no changes.

## Specification

``` clike
void Refresh(void)
```
