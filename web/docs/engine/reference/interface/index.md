---
title: Output Functions - the Keyboard API
---

The KeymanWeb core object `window.keyman.interface` (with legacy-oriented, deprecated alias `KeymanWeb`) exposes a number of functions for low-level processing of input, context and output. These functions are designed for use by keyboards compiled through Keyman Developer in order to facilitate input text processing and will also work for custom-coded KeymanWeb keyboards. As such, most of these functions should only be called by keyboard code, and a good understanding of the [Keyman Keyboard Language](/developer/language) will prove extremely beneficial toward understanding the keyboard API functions enumerated in this section.

Custom user interfaces would not normally use these functions, but they are described here as some custom keyboards, such as IME-style keyboards, may need to interact with the user interface.

[`any` Function (Deprecated)](any)
:   Returns whether or not the char `ch` is found within the [`any`](/developer/language/reference/any)([`store`](/developer/language/reference/store)) string, setting an internally-tracked index for use in the `indexOutput` function.
:   Shorthand name: `KeymanWeb.KA`

[`beep` Function](beep)
:   Flash body or element as substitute for an audible feedback [`beep`](/developer/language/reference/beep).
:   Shorthand name: `KeymanWeb.KB`

[`beepReset` Function](beepReset)
:   Cancels a previous feedback [`beep`](/developer/language/reference/beep) operation on a page element.
:   Shorthand name: `KeymanWeb.KBR`

[`context` Function (Deprecated)](context)
:   Gets [`context`](/developer/language/reference/context) for an ongoing keyboard operation relative to the caret's present position.
:   Shorthand name: `KeymanWeb.KC`

[`contextExOutput` Function](contextExOutput)
:   Emits the character or object at `contextOffset` from the current matched rule's context.
:   Shorthand name: `KeymanWeb.KCXO`

[`contextMatch` Function (Deprecated)](contextMatch)
:   Context matching: Returns `true` if the specified `context` call matches a provided string.
:   Shorthand name: `KeymanWeb.KCM`

[`deadkeyMatch` Function (Deprecated)](deadkeyMatch)
:   Deadkey matching: Seeks to match the [`deadkey`](/developer/language/reference/deadkey) state `dk` at the relative caret position `n`.
:   Shorthand name: `KeymanWeb.KDM`

[`deadkeyOutput` Function](deadkeyOutput)
:   Deadkey output: Associates the [`deadkey`](/developer/language/reference/deadkey) state `dk` with the element at the current caret position, after overwriting `nd` characters.
:   Shorthand name: `KeymanWeb.KDO`

[`deleteContext` Function](deleteContext)
:   Context deletion - removes the specified number of deadkeys and characters from the left of the caret.
:   Shorthand name: `KeymanWeb.KDC`

[`fullContextMatch` Function](fullContextMatch)
:   Context matching: Returns `true` if the current context matches the specified rule context specification.
:   Shorthand name: `KeymanWeb.KFCM`

[`ifStore` Function](ifStore)
:   `ifStore` compares the content of a [system `store`](/developer/language/guide/stores#toc-system-stores) with a string value.
:   Shorthand name: `KeymanWeb.KIFS`

[`indexOutput` Function](indexOutput)
:   Index-based output: Outputs a mapped character according to a previous selection from a `keyman.interface.any()` call upon a [`store`](/developer/language/reference/store) string, after deleting `nd` characters.
:   Shorthand name: `KeymanWeb.KIO`

[`insertText` Function](insertText)
:   Inserts a text string and optional [`deadkey`](/developer/language/reference/deadkey) into the active output element.
:   Shorthand name: `KeymanWeb.KT`

[`isKeypress` Function](isKeypress)
:   Returns `true` if the input event corresponds to a keypress event resulting in character output.
:   Shorthand name: `KeymanWeb.KIK`

[`keyInformation` Function](keyInformation)
:   Returns an object with extended information about a specified keystroke event.
:   Shorthand name: `KeymanWeb.KKI`

[`keyMatch` Function](keyMatch)
:   Keystroke matching: Returns `true` if the event matches the rule's shift mask and key code.
:   Shorthand name: `KeymanWeb.KKM`

[`loadStore` Function](loadStore)
:   Load an option [`store`](/developer/language/guide/stores) value from a cookie or default value if no prior stored value exists.
:   Shorthand name: `KeymanWeb.KLOAD`

[`nul` Function](_nul)
:   [`nul`](/developer/language/reference/nul) context check: Returns `true` if the length of the [`context`](/developer/language/reference/context) is less than or equal to `n` characters.
:   Shorthand name: `KeymanWeb.KN`

[`output` Function](output)
:   Outputs the specified string to an element, overwriting characters before the caret if specified.
:   Shorthand name: `KeymanWeb.KO`

[`registerKeyboard` Function](registerKeyboard)
:   Register the keyboard stub and load the keyboard.
:   Shorthand name: `KeymanWeb.KR`

[`registerStub` Function](registerStub)
:   Register the keyboard stub, return true if already registered.
:   Shorthand name: `KeymanWeb.KRS`

[`saveFocus` Function](saveFocus)
:   Save focus: Temporarily saves keyboard processing data for the currently-focused control.
:   Shorthand name: `KeymanWeb.KSF`

[`saveStore` Function](saveStore)
:   Save an option [`store`](/developer/language/guide/stores) value to a cookie for the active keyboard.
:   Shorthand name: `KeymanWeb.KSAVE`

[`setStore` Function](setStore)
:   `setStore` sets the value of a [system `store`](/developer/language/guide/stores#toc-system-stores) to a string.
:   Shorthand name: `KeymanWeb.KSETS`

[`stateMatch` Function](stateMatch)
:   State-key matching: Returns `true` if the event matches the rule's state-key requirements.
:   Shorthand name: `KeymanWeb.KSM`
