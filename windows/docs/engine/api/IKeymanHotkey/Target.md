---
title: IKeymanHotkey::Target Property
---

## Introduction

The `IKeymanHotkey::Target` property returns the target of the hotkey.
For a hotkey associated with a keyboard, this will always be
`khKeyboard`.

## Specification

``` clike
readonly KeymanHotkeyTarget Target
```

## Target Values

| Value | Target | Notes |
|----|----|----|
| 0 | khKeymanOff | Switches to first Windows keyboard which is not a Keyman keyboard |
| 1 | khKeyboardMenu | Opens the Keyman keyboard menu from the notification area |
| 2 | khVisualKeyboard | Opens the On Screen Keyboard page in the on screen keyboard. If the On Screen Keyboard page is already visible, closes the on screen keyboard |
| 3 | khKeymanConfiguration | Opens Keyman Configuration |
| 4 | khKeyboardUsage | Deprecated. Has no effect in Keyman 14 and later versions. |
| 5 | khFontHelper | Opens the Font Helper page in the on screen keyboard. If the Font Helper page is already visible, closes the on screen keyboard |
| 6 | khCharacterMap | Opens the Character Map page in the on screen keyboard. If the Character Map page is already visible, closes the on screen keyboard |
| 7 | khTextEditor | Opens the Keyman Text Editor |
| 8 | khLanguageSwitch | Opens the Language Switcher |
| 9 | khKeyboard | Selects or Toggles the keyboard associated with the hotkey. |
