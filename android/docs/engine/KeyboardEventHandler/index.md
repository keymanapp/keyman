---
title: KeyboardEventHandler class
---

## Description

The KeyboardEventHandler class provides keyboard events and methods to
notify registered listeners on keyboard and lexical-model events.

## Events

### OnKeyboardEventListener Interface

[`onKeyboardLoaded()`](onKeyboardLoaded)
:   is called when the keyboard has been loaded for the first time

[`onKeyboardChanged()`](onKeyboardChanged)
:   is called when another keyboard has been set

[`onKeyboardShown()`](onKeyboardShown)
:   is called when the keyboard has been shown

[`onKeyboardDismissed()`](onKeyboardDismissed)
:   is called when the keyboard has been dismissed

### OnKeyboardDownloadEventListener Interface

[`onKeyboardDownloadStarted()`](onKeyboardDownloadStarted)
:   is called when a keyboard download has started

[`onKeyboardDownloadFinished()`](onKeyboardDownloadFinished)
:   is called when a keyboard download has finished

[`onPackageInstalled()`](onPackageInstalled)
:   is called when a keyboard package has been installed

[`onLexicalModelInstalled()`](onLexicalModelInstalled)
:   is called when a lexical-model package has been installed
