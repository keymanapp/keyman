---
title: IKeymanKeyboard::LayoutType Property
---

## Introduction

The `IKeymanKeyboard::LayoutType` property returns the value of the
[`&mnemoniclayout`](/developer/language/reference/mnemoniclayout) store.
A keyboard layout that is mnemonic (`kltMnemonic`) is remapped by Keyman
Engine according to the current Latin script-based base layout, whereas
a keyboard layout that is positional (`kltPositional`) is not affected
by the current base layout.

## Specification

``` clike
readonly KeymanKeyboardLayoutType LayoutType
```

## Layout Types

| Code | Value | Description |
|----|----|----|
| kltPositional | 0 | The keyboard layout is not affected by the current base layout. |
| kltMnemonic | 1 | The keyboard layout has been remapped to match the current Latin script-based base layout. |
