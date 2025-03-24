---
title: Manager.addKeyboard()
---

## Summary

The **`addKeyboard()`** method adds a keyboard into the keyboards list.

**Deprecated** in favor of
[ResourceFileManager.shared.install()](../ResourceFileManager/install), which automatically includes the functionality of this method.

## Syntax

``` swift
Manager.addKeyboard(keyboard: InstallableKeyboard)
```

### Parameters

`keyboard`
:   The keyboard.

## Description

Use this method to include a keyboard in the keyboards list so that it
can be selected from the keyboards menu. If the keyboard with same
keyboard ID and language ID exists, it updates the existing keyboard
info.

## Examples

### Using `addKeyboard()`

The following script illustrates the use of `addKeyboard()`:

``` swift
// Add a custom keyboard
let kb = InstallableKeyboard(id: "tamil99m",
                             name: "Tamil 99M",
                             languageID: "ta",
                             languageName: "Tamil",
                             version: "1.1",
                             isRTL: false,
                             font: Font(filename: "aava1.ttf"),
                             oskFont: nil,
                             isCustom: true)
Manager.shared.addKeyboard(Defaults.keyboard)
Manager.shared.addKeyboard(kb)
```

## History

Deprecated in Keyman Engine for iPhone and iPad 14.0.

## See also

-   [`removeKeyboard()`](removeKeyboard)
