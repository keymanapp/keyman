---
title: IKeymanControl::ActiveLanguage Property
---

## Introduction

The `IKeymanControl::ActiveLanguage` property sets or returns the last
active [`IKeymanLanguage`](../IKeymanLanguage) for the current thread.

This is a convenience wrapper for the Windows
`ITfInputProcessorProfileMgr::ActivateProfile` method.

## Specification

``` clike
IKeymanLanguage* ActiveLanguage
```
