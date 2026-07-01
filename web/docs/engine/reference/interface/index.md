---
title: `keyman.interface` - the Keyboard API
---

The KeymanWeb core object `window.keyman.interface` exposes a number of
functions for low-level processing of input, context and output. These functions
are designed for use by keyboards compiled through Keyman Developer in order to
facilitate input text processing and will also work for custom-coded KeymanWeb
keyboards. These functions should only be called by keyboard code, and a good
understanding of the [Keyman Keyboard Language](/developer/language) will prove
extremely beneficial toward understanding the keyboard API functions enumerated
in this section.

The KeymanWeb Keyboard API publishes a deprecated global `KeymanWeb` which is
the same as `keyman.interface`. Use `keyman.interface` in preference to
`KeymanWeb`.

Custom user interfaces should not use these functions. These functions are intended
for use in a keyboard context, in particular during a keystroke event cycle.

[`any` Function (Deprecated)](any)
:   Returns whether or not the char `ch` is found within the [`any`](/developer/language/reference/any)([`store`](/developer/language/reference/store)) string, setting an internally-tracked index for use in the `indexOutput` function.
:   Shorthand name: `keyman.interface.KA`

[`beep` Function](beep)
:   Flash body or element as substitute for an audible feedback [`beep`](/developer/language/reference/beep).
:   Shorthand name: `keyman.interface.KB`

[`beepReset` Function](beepReset)
:   Cancels a previous feedback [`beep`](/developer/language/reference/beep) operation on a page element.
:   Shorthand name: `keyman.interface.KBR`

[`context` Function (Deprecated)](context)
:   Gets [`context`](/developer/language/reference/context) for an ongoing keyboard operation relative to the caret's present position.
:   Shorthand name: `keyman.interface.KC`

[`contextExOutput` Function](contextExOutput)
:   Emits the character or object at `contextOffset` from the current matched rule's context.
:   Shorthand name: `keyman.interface.KCXO`

[`contextMatch` Function (Deprecated)](contextMatch)
:   Context matching: Returns `true` if the specified `context` call matches a provided string.
:   Shorthand name: `keyman.interface.KCM`

[`deadkeyMatch` Function (Deprecated)](deadkeyMatch)
:   Deadkey matching: Seeks to match the [`deadkey`](/developer/language/reference/deadkey) state `dk` at the relative caret position `n`.
:   Shorthand name: `keyman.interface.KDM`

[`deadkeyOutput` Function](deadkeyOutput)
:   Deadkey output: Associates the [`deadkey`](/developer/language/reference/deadkey) state `dk` with the element at the current caret position, after overwriting `nd` characters.
:   Shorthand name: `keyman.interface.KDO`

[`deleteContext` Function](deleteContext)
:   Context deletion - removes the specified number of deadkeys and characters from the left of the caret.
:   Shorthand name: `keyman.interface.KDC`

[`focusLastActiveTextStore` Function](focusLastActiveTextStore)
:  Set focus to the currently-focused or most recently focused text store.

[`fullContextMatch` Function](fullContextMatch)
:   Context matching: Returns `true` if the current context matches the specified rule context specification.
:   Shorthand name: `keyman.interface.KFCM`

[`getLastActiveTextStore` Function](getLastActiveTextStore)
:  Get the currently or most recently focused text store. This is for use by IME keyboards.

[`ifStore` Function](ifStore)
:   `ifStore` compares the content of a [system `store`](/developer/language/guide/stores#toc-system-stores) with a string value.
:   Shorthand name: `keyman.interface.KIFS`

[`indexOutput` Function](indexOutput)
:   Index-based output: Outputs a mapped character according to a previous selection from a `keyman.interface.any()` call upon a [`store`](/developer/language/reference/store) string, after deleting `nd` characters.
:   Shorthand name: `keyman.interface.KIO`

[`insertText` Function](insertText)
:   Inserts a text string and optional [`deadkey`](/developer/language/reference/deadkey) into the active output element.
:   Shorthand name: `keyman.interface.KT`

[`isKeypress` Function](isKeypress)
:   Returns `true` if the input event corresponds to a keypress event resulting in character output.
:   Shorthand name: `keyman.interface.KIK`

[`keyInformation` Function](keyInformation)
:   Returns an object with extended information about a specified keystroke event.
:   Shorthand name: `keyman.interface.KKI`

[`keyMatch` Function](keyMatch)
:   Keystroke matching: Returns `true` if the event matches the rule's shift mask and key code.
:   Shorthand name: `keyman.interface.KKM`

[`loadStore` Function](loadStore)
:   Load an option [`store`](/developer/language/guide/stores) value from a cookie or default value if no prior stored value exists.
:   Shorthand name: `keyman.interface.KLOAD`

[`nul` Function](_nul)
:   [`nul`](/developer/language/reference/nul) context check: Returns `true` if the length of the [`context`](/developer/language/reference/context) is less than or equal to `n` characters.
:   Shorthand name: `keyman.interface.KN`

[`output` Function](output)
:   Outputs the specified string to an element, overwriting characters before the caret if specified.
:   Shorthand name: `keyman.interface.KO`

[`registerKeyboard` Function](registerKeyboard)
:   Register the keyboard stub and load the keyboard.
:   Shorthand name: `keyman.interface.KR`

[`registerStub` Function](registerStub)
:   Register the keyboard stub, return true if already registered.
:   Shorthand name: `keyman.interface.KRS`

[`saveFocus` Function](saveFocus)
:   Save focus: Temporarily saves keyboard processing data for the currently-focused control.
:   Shorthand name: `keyman.interface.KSF`

[`saveStore` Function](saveStore)
:   Save an option [`store`](/developer/language/guide/stores) value to a cookie for the active keyboard.
:   Shorthand name: `keyman.interface.KSAVE`

[`setStore` Function](setStore)
:   `setStore` sets the value of a [system `store`](/developer/language/guide/stores#toc-system-stores) to a string.
:   Shorthand name: `keyman.interface.KSETS`

[`stateMatch` Function](stateMatch)
:   State-key matching: Returns `true` if the event matches the rule's state-key requirements.
:   Shorthand name: `keyman.interface.KSM`

## Deprecated functions for IMEs

The following functions have been retained for compatibility with existing IME
keyboards, but should not be used in any new keyboards or user interfaces.

[`GetLastActiveElement` or `getLastActiveElement` Function (Deprecated)](GetLastActiveElement)
:   Use [`keyman.interface.getLastActiveTextStore()`](getLastActiveTextStore)

[`FocusLastActiveElement` or `focusLastActiveElement` Function (Deprecated)](FocusLastActiveElement)
:   Use [`keyman.interface.focusLastActiveTextStore()`](focusLastActiveTextStore)

[`HideHelp` or `hideHelp` Function (Deprecated)](HideHelp)
:   Use [`keyman.osk.hide()`](../osk/hide)

[`ShowHelp` or `showHelp` Function (Deprecated)](ShowHelp)
:   Use [`keyman.osk.setPos()`](../osk/setPos)
    Use [`keyman.osk.show()`](../osk/show)

[`ShowPinnedHelp` or `showPinnedHelp` Function (Deprecated)](ShowPinnedHelp)
:   Use [`keyman.osk.setRect()`](../osk/setRect)
:   Use [`keyman.osk.show()`](../osk/show)
