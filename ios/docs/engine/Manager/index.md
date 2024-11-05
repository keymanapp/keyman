---
title: Manager class
---

## Summary

The **Manager** class provides methods and properties for controlling Keyman Engine

## Description

The Manager is the core class which provides most of the methods, properties, and constants you will need to develop your apps with Keyman Engine

## Properties

[`spacebarText`](spacebarText)

: controls the default text pattern shown on the spacebar

## Methods

[`addKeyboard()`](addKeyboard)

: adds a keyboard into the keyboards list

[`canAddNewKeyboards`](canAddNewKeyboards)

: returns whether adding a new keyboard is enabled in the keyboard picker menu

[`currentKeyboard()`](currentKeyboard)

: returns info for the currently selected keyboard

[`dismissKeyboardPicker()`](dismissKeyboardPicker)

: hides the list of keyboards

[`downloadKeyboard()`](downloadKeyboard)

: asynchronously fetches the keyboard files

[`downloadLexicalModel()`](downloadKeyboard)

: asynchronously fetches the dictionary files

[`fontNameForKeyboard()`](fontNameForKeyboard)

: returns the font name for the given keyboard ID and language ID

[`hideKeyboard()`](hideKeyboard)

: hides the in-app OSK

[`keyboardHeight()`](keyboardHeight)

: returns the height of the keyboard

[`removeKeyboard()`](removeKeyboard)

: removes the keyboard at specified position from the keyboards list

[`setKeyboard()`](setKeyboard)

: sets the keyboard to be used

[`showKeyboard()`](showKeyboard)

: show the in-app OSK

[`showKeyboardPicker()`](showKeyboardPicker)

: displays a list of available keyboards and allows a user to add/remove keyboards

[`stateForKeyboard()`](stateForKeyboard)

: returns the current state of availability for a keyboard

[`stateForLexicalModel()`](stateForLexicalModel)

: returns the current state of availability for a dictionary

[`switchToNextKeyboard()`](switchToNextKeyboard)

: loads the next available keyboard in keyboards list

