---
title: IKeymanHotkeys::Items[Index] Property
---

## Introduction

The `IKeymanHotkeys::Items[Index]` property returns an
[`IKeymanHotkey`](../IKeymanHotkey) reference to the hotkey by `Index`.
If the hotkey is not found, exception
[`KMN_E_Collection_InvalidIndex`](../IKeymanError/ErrorCode) is raised.
The `Index` is the enumerated type `KeymanHotkeyTarget`.

## Specification

``` clike
readonly IKeymanHotkey* Items[KeymanHotkeyTarget Index]
```

## Parameters

Index
:   The [target](../IKeymanHotkey/Target) of the hotkey.
