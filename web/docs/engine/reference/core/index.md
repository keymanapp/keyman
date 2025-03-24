---
title: Core Module
---

The KeymanWeb core module is exposed to the developer as `window.keyman`.

[`build` Property](build)
: The release build of KeymanWeb.


[`version` Property](version)
: The version of KeymanWeb.


[`initialized` Property](initialized)
: Keymanweb core module initialization state.


[`activatingUI` Function](activatingUI)
: Set an internal flag to notify KeymanWeb of change in UI activation state.


[`addEventListener` Function](addEventListener)
: Adds an event listener for user-handling of keymanweb events.


[`addHotKey` Function](addHotKey)
: Add hot key handler to array of document-level hotkeys triggered by key-up event.


[`addKeyboards` Function](addKeyboards)
: Adds keyboards to keymanweb.


[`addKeyboardsForLanguage` Function](addKeyboardsForLanguage)
: Adds default or all keyboards for a given language to keymanweb.


[`attachToControl` Function](attachToControl)
: Attach KeymanWeb to HTML element (or IFrame).


[ `BuildVisualKeyboard` Function](BuildVisualKeyboard)
: Create a copy of the OSK for embedding in documentation or help page.


[`detachFromControl` Function](detachFromControl)
: Detach KeymanWeb from HTML element (or IFrame).


[`disableControl` Function](disableControl)
: Disables KeymanWeb input handling for the specified control.


[`enableControl` Function](enableControl)
: Enables KeymanWeb input handling for the specified control.


[`focusLastActiveElement` Function](focusLastActiveElement)
: Restore the focus to the element active before input was moved to KeymanWeb.


[`getActiveKeyboard` Function](getActiveKeyboard)
: Get the ID (internal name) of the currently active keyboard.


[`getActiveLanguage` Function](getActiveLanguage)
: Get the language code for the currently selected language.


[`getKeyboard` Function](getKeyboard)
: Get keyboard meta data for the selected keyboard and language.


[`getKeyboardForControl` Function](getKeyboardForControl)
: Obtain the keyboard set for a specific control, if it exists.


[`getKeyboards` Function](getKeyboards)
: Get details of currently installed keyboards.


[`getLastActiveElement` Function](getLastActiveElement)
: Return the last element activated before input was moved to KeymanWeb.


[`getSavedKeyboard` Function](getSavedKeyboard)
: Get the (internal) keyboard name and language code of the most recently active
  keyboard.


[`getUIState` Function](getUIState)
: Get the KeymanWeb user interface activation state.


[`init` Function](init)
: Sets license key, selects user interface, and other KeymanWeb Options.


[`isChiral` Function](isChiral)
: Test if a given keyboard recognizes chiral modifier data, such as left-control vs right-control.


[`isCJK` Function](isCJK)
: Test if a given keyboard or keyboard stub (or the current keyboard) is for Chinese, Japanese, or Korean.


[`moveToElement` Function](moveToElement)
: Move input focus to user specified element.


[`removeEventListener` Function](removeEventListener)
: Removes a user-defined event handler.


[`removeHotKey` Function](removeHotKey)
: Remove hotkey handler from document's list of hotkey handlers.


[`removeKeyboards` Function](removeKeyboards)
: Removes keyboards (by ID) from KeymanWeb.


[`resetContext` Function](resetContext)
: Revert OSK to default layer and clear any deadkeys and modifiers


[`setActiveKeyboard` Function](setActiveKeyboard)
: Change the currently active keyboard.


[`setKeyboardForControl` Function](setKeyboardForControl)
: Associate control with independent keyboard settings initialized to a specific keyboard.
