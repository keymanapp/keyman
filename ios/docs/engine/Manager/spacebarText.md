---
title: Manager.spacebarText
---

## Summary

The `spacebarText` property gets or sets the current text display pattern for
the spacebar.

## Syntax

```swift
Manager.SpacebarText pattern = Manager.spacebarText
Manager.spacebarText = pattern
```

### Returns

`Manager.SpacebarText` is an enum with the following members:

  * `LANGUAGE` - the language name for the keyboard
  * `KEYBOARD` - the keyboard name
  * `LANGUAGE_KEYBOARD` - both the language name and the keyboard name,
    separated by hyphen
  * `BLANK` - no text to be displayed

## Description

The default text display pattern is `LANGUAGE_KEYBOARD`. The text shown on the
keyboard may be overridden on a per-keyboard basis with the `displayName` member
of `InstallableKeyboard`.
