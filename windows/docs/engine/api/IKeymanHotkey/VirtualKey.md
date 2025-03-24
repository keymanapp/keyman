---
title: IKeymanHotkey::VirtualKey Property
---

## Introduction

The `IKeymanHotkey::VirtualKey` property sets or returns the virtual key
associated with the hotkey.

Changes to the hotkey will be applied after
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
is called for a keyboard hotkey, or after
[`IKeymanHotkeys::Apply`](../IKeymanHotkeys/Apply) is called for other
hotkeys.

## Specification

``` clike
long VirtualKey
```
