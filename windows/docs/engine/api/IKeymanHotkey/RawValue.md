---
title: IKeymanHotkey::RawValue Property
---

## Introduction

The `IKeymanHotkey::RawValue` property sets or returns the raw hotkey
value, which is the [`Modifiers`](Modifiers) property bitwise or-ed with
the [`VirtualKey`](VirtualKey) property.

Changes to the hotkey will be applied after
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
is called for a keyboard hotkey, or after
[`IKeymanHotkeys::Apply`](../IKeymanHotkeys/Apply) is called for other
hotkeys.

## Specification

``` clike
long RawValue
```
