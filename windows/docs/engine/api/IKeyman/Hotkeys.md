---
title: IKeyman::Hotkeys Property
---

## Introduction

The `IKeyman::Hotkeys` property returns the Keyman
[`IKeymanHotkeys`](../IKeymanHotkeys) interface which provides a list of
configured user interface hotkeys. Hotkeys for activating a specific
keyboard layout are not listed here; these are visible under the
[`IKeymanLanguage::Hotkey`](../IKeymanLanguage/Hotkey) property.

## Specification

``` clike
readonly IKeymanHotkeys* Hotkeys
```
