---
title: IKeymanHotkey::Clear() Method
---

## Introduction

The `IKeymanHotkey::Clear()` method clears the hotkey value.

Changes to the hotkey will be applied after
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
is called for a keyboard hotkey, or after
[`IKeymanHotkeys::Apply`](../IKeymanHotkeys/Apply) is called for other
hotkeys.

## Specification

``` clike
void Clear(void)
```
