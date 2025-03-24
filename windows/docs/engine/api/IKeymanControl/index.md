---
title: IKeymanControl Interface
---

## Introduction

The `IKeymanControl` interface controls the operation of Keyman Engine
and provides methods to trigger Keyman Engine's user interface.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanControl`**  


## Properties

[`ActiveLanguage`](ActiveLanguage)
:   Returns or sets the currently active keyboard + language.

<!-- -->

[`LastActiveWindow`](LastActiveWindow) <span class="readonly">read only</span>
:   Returns the last active window handle, excluding Keyman windows such
    as the On Screen Keyboard.

<!-- -->

[`LastFocusWindow`](LastFocusWindow) <span class="readonly">read only</span>
:   Returns the last focus window handle, irrespective of process.

## Methods

[`IsConfigurationOpen()`](IsConfigurationOpen)
:   Returns `True` if Keyman Configuration is currently running.

<!-- -->

[`IsKeymanRunning()`](IsKeymanRunning)
:   Returns `True` if Keyman Engine is currently running.

<!-- -->

[`IsOnlineUpdateCheckOpen()`](IsOnlineUpdateCheckOpen)
:   Returns `True` if the Keyman Desktop online update check tool is
    currently running.

<!-- -->

[`IsTextEditorOpen()`](IsTextEditorOpen)
:   Returns `True` if the Keyman Desktop text editor is currently
    running.

<!-- -->

[`IsVisualKeyboardOpen()`](IsVisualKeyboardOpen)
:   Returns `True` if the on screen keyboard is running.

<!-- -->

[`OpenConfiguration()`](OpenConfiguration)
:   Starts Keyman Configuration.

<!-- -->

[`OpenDiagnostics()`](OpenDiagnostics)
:   Opens the Keyman diagnostics tool.

<!-- -->

[`OpenHelp()`](OpenHelp)
:   Opens Keyman Desktop help at the specific topic.

<!-- -->

[`OpenTextEditor()`](OpenTextEditor)
:   Opens Keyman Desktop text editor.

<!-- -->

[`OpenUpdateCheck()`](OpenUpdateCheck)
:   Opens Keyman Desktop online update check tool.

<!-- -->

[`ShowKeyboardWelcome()`](ShowKeyboardWelcome)
:   Loads the welcome.htm file in a window for the selected keyboard.

<!-- -->

[`StartKeyman()`](StartKeyman)
:   Starts Keyman Engine, if it isn't already running.

<!-- -->

[`StartVisualKeyboard()`](StartVisualKeyboard)
:   Starts the on screen keyboard, if it isn't already visible.

<!-- -->

[`StopKeyman()`](StopKeyman)
:   Stops Keyman Engine.

<!-- -->

[`StopVisualKeyboard()`](StopVisualKeyboard)
:   Stops the on screen keyboard.
