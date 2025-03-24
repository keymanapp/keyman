---
title: KMManager class
---

## Summary

The **`KMManager`** class provides methods for controlling Keyman Engine

## Syntax

```java
KMManager.methodName()
```
```java
KMManager.CONSTANT
```

## Description

The KMManager is the core class which provides most of the methods and constants you will need to develop your apps with Keyman Engine

<!-- Fill in examples of how to use, etc. -->

## Methods

[`addKeyboard()`](addKeyboard)
: adds a keyboard into the keyboards list

[`addKeyboardDownloadEventListener()`](addKeyboardDownloadEventListener)
: adds the specified listener into the list of keyboard download event listeners

[`addKeyboardEventListener()`](addKeyboardEventListener)
: adds the specified listener into the list of keyboard event listeners

[`addLexicalModel()`](addLexicalModel)
: adds a lexical model into the lexical models list

[`advanceToNextInputMode()`](advanceToNextInputMode)
: switch to the next system keyboard input mode

[`advanceToPreviousInputMethod()`](advanceToPreviousInputMethod)
: switch to the previous system keyboard input mode

[`applyKeyboardHeight()`](applyKeyboardHeight)
: sets the height of keyboard frame

[`canAddNewKeyboard()`](canAddNewKeyboard)
: returns whether adding a new keyboard is enabled, like in the keyboard picker menu

[`canRemoveKeyboard()`](canRemoveKeyboard)
: returns whether removing a keyboard is enabled, like in the keyboard picker menu

[`copyHTMLBannerAssets()`](copyHTMLBannerAssets)
: copies a folder of HTML banner assets so it's available for your keyboard app's resources

[`createInputView()`](createInputView)
: creates the input view to be used in InputMethodService

[`deregisterLexicalModel()`](deregisterLexicalModel)
: deregisters the specified lexical model from the LMLayer so it isn't used

[`executeHardwareKeystroke()`](executeHardwareKeystroke)
: process the keystroke generated from a physical keyboard

[`getAssociatedLexicalModel()`](getAssociatedLexicalModel)
: search the installed lexical models list and see if there's an associated model for a given language

[`getBannerHeight()`](getBannerHeight)
: returns the height of the suggestion banner

[`getCurrentKeyboardIndex()`](getCurrentKeyboardIndex)
: returns index number of the current keyboard in keyboards list

[`getCurrentKeyboardInfo()`](getCurrentKeyboardInfo)
: returns information dictionary of the current keyboard

[`getDefaultKeyboard()`](getDefaultKeyboard)
: returns the keyboard information for the fallback keyboard

[`getFontTypeface()`](getFontTypeface)
: creates a new typeface from the specified font filename

[`getGlobeKeyAction()`](getGlobeKeyAction)
: returns the action type of the 'Globe' key

[`getHapticFeedback()`](getHapticFeedback)
: returns whether the device vibrates as the user types

~~`getKeyboardFontFilename()`~~ `(Deprecated)`
: use getKeyboardTextFontFilename or getKeyboardOskFontFilename instead

~~`getKeyboardFontTypeface()`~~ `(Deprecated)`
: use getKeyboardTextFontTypeface or getKeyboardOskFontTypeface instead

[`getKeyboardHeight()`](getKeyboardHeight)
: returns the height of the keyboard frame

[`getKeyboardIndex()`](getKeyboardIndex)
: returns index number of the specified keyboard in keyboards list

[`getKeyboardInfo()`](getKeyboardInfo)
: returns information dictionary of the specified keyboard

[`getKeyboardsList()`](getKeyboardsList)
: returns the array of keyboards list

[`getKeyboardOskFontFilename()`](getKeyboardOskFontFilename)
: returns the selected keyboard's OSK font filename

[`getKeyboardOskFontTypeface()`](getKeyboardOskFontTypeface)
: creates a new typeface from the selected keyboard's OSK font

[`getKeyboardState()`](getKeyboardState)
: returns the specified keyboard's state

[`getKeyboardTextFontFilename()`](getKeyboardTextFontFilename)
: returns the selected keyboard's text font filename

[`getKeyboardTextFontTypeface()`](getKeyboardTextFontTypeface)
: creates a new typeface from the selected keyboard's text font

[`getKMKeyboard()`](getKMKeyboard)
: returns the KMKeyboard depending whether it's an in-app or system keyboard

[`getLatestKeyboardFileVersion()`](getLatestKeyboardFileVersion)
: returns the specified keyboard's latest file version number

[`getLexicalModelInfo()`](getLexicalModelInfo)
: returns dictionary information of the specified lexical model

[`getLexicalModelsList()`](getLexicalModelsList)
: returns the array of lexical models list

[`getLanguageCorrectionPreferenceKey()`](getLanguageCorrectionPreferenceKey)
: returns a String to use as a shared preference key to store whether the LMLayer should enable corrections for a given language

[`getLanguagePredictionPreferenceKey()`](getLanguagePredictionPreferenceKey)
: returns a String to use as a shared preference key to store whether the LMLayer should enable suggestions for a given language.

[`getLongpressDelay()`](getLongpressDelay)
: returns from stored preference the number of milliseconds to trigger a longpress gesture

[`getMaySendCrashReport()`](getMaySendCrashReport)
: returns whether Keyman Engine is allowed to send crash reports over the network to sentry.keyman.com

[`getOrientation()`](getOrientation)
: returns the device's current orientation (Portrait vs Landscape)

[`getSpacebarText()`](getSpacebarText)
: returns the current text display pattern for the spacebar

[`getVersion()`](getVersion)
: returns the version number of Keyman Engine

[`getWindowDensity()`](getWindowDensity)
: returns the density of the window

[`getWindowSize()`](getWindowSize)
: returns the size of an area the window would occupy

[`hasConnection()`](hasConnection)
: returns whether the device has active network connection

[`hideSystemKeyboard()`](hideSystemKeyboard)
: hides the system OSK

[`initialize()`](initialize)
: initializes the Keyman manager

[`isDebugMode()`](isDebugMode)
: returns whether debugging of Keyman Engine is enabled

~~`isHelpBubbleEnabled()`~~ `(Deprecated)`
: returns whether the help bubble is enabled

[`isKeyboardLoaded()`](isKeyboardLoaded)
: returns whether the specified in-app or system keyboard is loaded

[`keyboardExists()`](keyboardExists)
: returns whether the specified keyboard exists in keyboards list

[`lexicalModelExists()`](lexicalModelExists)
: returns whether the specified lexical model exists in lexical models list

[`onConfigurationChanged()`](onConfigurationChanged)
: performs necessary actions in an InputMethodService's <code>onConfigurationChanged()</code>

[`onDestroy()`](onDestroy)
: performs necessary actions in an InputMethodService's <code>onDestroy()</code>

[`onPause()`](onPause)
: performs necessary actions in an Activity's <code>onPause()</code>

[`onResume()`](onResume)
: performs necessary actions in an Activity's <code>onResume()</code>

[`onStartInput()`](onStartInput)
: performs necessary actions in an InputMethodService's <code>onStartInput()</code>

[`registerAssociatedLexicalModel()`](registerAssociatedLexicalModel)
: registers a lexical model with the associated language ID

[`registerLexicalModel()`](registerLexicalModel)
: registers a lexical model to use with the LMLayer

[`removeKeyboard()`](removeKeyboard)
: removes the keyboard at specified position from the keyboards list

[`removeKeyboardDownloadEventListener()`](removeKeyboardDownloadEventListener)
: removes the specified listener from the list of keyboard download event listeners

[`removeKeyboardEventListener()`](removeKeyboardEventListener)
: removes the specified listener from the list of keyboard event listeners

[`sendOptionsToKeyboard()`](sendOptionsToKeyboard)
: sends options like longpress delay to the KeymanWeb keyboard

[`setCanAddNewKeyboard()`](setCanAddNewKeyboard)
: sets whether adding a new keyboard is allowed

[`setCanRemoveKeyboard()`](setCanRemoveKeyboard)
: sets whether removing a keyboard is allowed, like in the keyboard picker menu

[`setDebugMode()`](setDebugMode)
: enables or disables debugging of Keyman Engine

[`setDefaultKeyboard()`](setDefaultKeyboard)
: sets the keyboard information for the fallback keyboard

[`setGlobeKeyAction()`](setGlobeKeyAction)
: sets an action type for the 'Globe' key

[`setHapticFeedback()`](setHapticFeedback)
: sets whether the device vibrates as the user types

~~`setHelpBubbleEnabled()`~~ `(deprecated)`
: enables or disables the help bubble

[`setHTMLBanner()`](setHTMLBanner)
: sets the contents of an HTML banner for Keyman Engine to display when suggestions aren't available

[`setKeyboard()`](setKeyboard)
: sets the keyboard to be used

[`setKeyboardPickerFont()`](setKeyboardPickerFont)
: sets the font for the keyboard picker menu

~~`setKeymanLicense()`~~ `(Deprecated)`
: sets the developer license/key pair to unlock Keyman Engine

[`setLongpressDelay()`](setLongpressDelay)
: stores the longpress delay in milliseconds as a preference.

[`setMaySendCrashReport()`](setMaySendCrashReport)
: sets whether Keyman Engine can send crash reports over the network to sentry.keyman.com

[`setShouldAllowSetKeyboard()`](setShouldAllowSetKeyboard)
: sets whether Keyman Engine allows setting a keyboard other than the default keyboard

[`setShouldCheckKeyboardUpdates()`](setShouldCheckKeyboardUpdates)
: sets whether Keyman Engine should check for keyboard updates

[`setSpacebarText()`](setSpacebarText)
: sets the current text display pattern for the spacebar

[`shouldAllowSetKeyboard()`](shouldAllowSetKeyboard)
: returns whether Keyman Engine allows setting a keyboard other than the default keyboard

[`shouldCheckKeyboardUpdates()`](shouldCheckKeyboardUpdates)
: returns whether Keyman Engine should check for keyboard updates

[`showKeyboardPicker()`](showKeyboardPicker)
: displays the keyboard picker menu

~~`showLanguageList()`~~ `(Deprecated)`
: displays the language list

[`switchToNextKeyboard()`](switchToNextKeyboard)
: loads the next available keyboard in keyboards list

[`updateSelectionRange()`](updateSelectionRange)
: updates the selection range of the current context

[`updateText()`](updateText)
: updates the current context with the specified text
