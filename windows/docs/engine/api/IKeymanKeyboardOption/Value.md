---
title: IKeymanKeyboardOption::Value Property
---

## Introduction

The `IKeymanKeyboardOption::Value` property controls the current value
of the option. Valid values depend on the keyboard layout.

Changes to this value will be applied after
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
is called.

## Specification

``` clike
string Value
```
