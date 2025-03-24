---
title: IKeymanKeyboardInstalled::Options Property
---

## Introduction

The `IKeymanKeyboardInstalled::Options` property returns an
[`IKeymanKeyboardOptions`](../IKeymanKeyboardOptions) reference which
lists the user-configurable options for the keyboard layout. Changes to
this collection will be applied after
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
is called.

## Specification

``` clike
readonly IKeymanKeyboardOptions* Options
```
