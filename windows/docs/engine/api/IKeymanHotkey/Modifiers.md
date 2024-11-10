---
title: IKeymanHotkey::Modifiers Property
---

## Introduction

The `IKeymanHotkey::Modifiers` property controls the set of modifiers
associated with the hotkey.

Changes to the hotkey will be applied after
[`IKeymanKeyboardsInstalled::Apply`](../IKeymanKeyboardsInstalled/Apply)
is called for a keyboard hotkey, or after
[`IKeymanHotkeys::Apply`](../IKeymanHotkeys/Apply) is called for other
hotkeys.

## Specification

``` clike
KeymanHotkeyModifiers Modifiers
```

## Modifier Values

| Value   | Modifier |
|---------|----------|
| 0x10000 | HK_ALT   |
| 0x20000 | HK_CTRL  |
| 0x40000 | HK_SHIFT |
