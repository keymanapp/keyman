---
title: IKeymanKeyboardInstalled::Loaded Property
---

## Introduction

The `IKeymanKeyboardInstalled::Loaded` property controls whether or not
the keyboard is currently available for use and visible to the user in
the keyboard picker. When this property is changed,
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
must be called to apply the setting to Keyman Engine.

## Specification

``` clike
bool Loaded
```
