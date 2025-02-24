---
title: Keyman for iPhone and iPad Developer Support
---

## Overview

The Keyman Engine for iPhone and iPad 19.0 SDK is designed to provide
advanced international keyboard support to iOS apps.

As a developer, you simply need to use (or subclass) TextView or
TextField instead of using an ordinary UITextView or UITextField.

To let a user pick/download additional keyboards, there are 2 pre-made
buttons you can use that bring up the Keyman keyboard UI.

All of the methods and delegate calls of the original UIKit classes are
retained.

As of version 14.0, we deprecated direct use of .js files for resources
in favor of supporting .kmp packages. Package files have the same old
.js files bundled alongside any related resources (like fonts and
documentation) on your behalf, making the process of setting up
keyboards and lexical models easier.

## Guides

- [Build an in-app keyboard](guides/in-app/)
- [Build a system keyboard app](guides/system-keyboard/)

## Available Classes/Methods

### Resource Identification

[`LanguageResourceFullID`](LanguageResource/LanguageResourceFullID)
:   The LanguageResourceFullID protocol and its implementing types
    `FullKeyboardID` and `LexicalModelFullID` serve as unique
    identifiers for language resources, whether installed or
    uninstalled.

[`LanguageResource`](LanguageResource)
:   The LanguageResource protocol and its implementing types
    `InstallableKeyboard` and `InstallableLexicalModel` represent the
    full metadata specification for a single installed pairing of
    keyboard or model & target language.

[`KeymanPackage`](KeymanPackage/)
:   The KeymanPackage is the base class used to represent various types
    of Keyman packages within the Keyman Engine and their contents. This
    class also serves a "type erasure" role for classes derived from it.

[`TypedKeymanPackage`](TypedKeymanPackage/)
:   The TypedKeymanPackage (and its direct subclasses
    `KeyboardKeymanPackage` and `LexicalModelKeymanPackage`) provide
    enhanced type signatures.

### Resource Management

[`ResourceFileManager`](ResourceFileManager/)
:   The core class for managing and installing keyboards and lexical models

### Controlling the Keyboard

[`Manager`](Manager/)
:   The core class for controlling Keyman Engine

### UI elements & setup

[`TextField`](TextField/)
:   A UITextField to use with the Keyman Engine

[`TextView`](TextView/)
:   A UITextView to use with the Keyman Engine

[`KeyboardPickerButton`](KeyboardPickerButton/)
:   A button for bringing up the keyboard picker menu

[`KeyboardPickerBarButtonItem`](KeyboardPickerBarButtonItem/)
:   A bar that displays the keyboard picker when tapped

[`PackageWebViewController`](PackageWebViewController/)
:   Displays web pages contained within packages, ejecting any links outside the package into an external browser.

## Adding the Keyman Engine for iPhone and iPad SDK to your project

1) Create a new iOS project

2) On your project's "General" tab, click the '+' sign under "Embedded Binaries" to add KeymanEngine.framework.

-   If you wish to use KeymanEngine-universal.framework instead for development, be aware that you may need to note the following:
    -   Xcode may have issues with the "-universal" part of the name; dropping this component after copying may be prudent.
    -   The 16.0 version of Keyman Engine for iPhone and iPad was compiled using Swift 5.
    -   Dragging the framework into the project will not properly set the framework up for embedding.
-   When done properly, Xcode should automatically link the library and setup the bundle for copying.
    - To verify, go to your target's "Build Phases" tab and check that the following details are in place:
        -   'KeymanEngine.framework' is set within "Link Binary With Libraries"
        -   'KeymanEngine.framework' is set within "Embed Frameworks"

3) Include KeymanEngine's dependencies within your project:
(found in 'Link Binary With Libraries' in your target's 'build phases'
tab)

-   Ensure each of the following frameworks is included under "Embedded
    Binaries":
    -   DeviceKit
    -   ObjcExceptionBridging
    -   Reachability
    -   Sentry
    -   XCGLogger
    -   Zip
-   You may find it helpful to use a dependency manager (such as
    [Carthage](https://github.com/Carthage/Carthage)) to assist with
    framework maintenance. You may find our Cartfile (used with
    Carthage) for this
    [here](https://github.com/keymanapp/keyman/blob/master/ios/Cartfile).

4) Import "KeymanEngine" into any source files requiring Keyman
classes.

5) [OPTIONAL] Add the language .kmp files which you want to use to
your project (e.g. thai_kedmanee.kmp)

-   drag them into the project
-   go to your target, then the "Build Phases" tab, and add them to the
    "Copy Bundle Resources" section
-   to avoid xcode warnings, remove them from the "Compile Sources"
    section

6) [OPTIONAL] Set the initial language prior to showing the keyboard
for the first time

-   it is recommended to do this when the app first launches

-   e.g. for Thai Kedmanee

    ```swift
    Manager.shared.setKeyboard("thai_kedmanee");
    ```

-   the keyboard defaults to English USA if not set otherwise

## Keyman Demo

To see the SDK in action, open the samples folder to find two small
sample projects. All of the above steps have already been performed.

## See also

-   [Keyman Engine for iPhone and iPad 13.0](/developer/engine/iphone-and-ipad/13.0/)
-   [Keyman Engine for iPhone and iPad 12.0](/developer/engine/iphone-and-ipad/12.0/)
-   [Keyman Engine for iPhone and iPad 11.0](/developer/engine/iphone-and-ipad/11.0/)
-   [Keyman Engine for iPhone and iPad 10.0](/developer/engine/iphone-and-ipad/10.0/)
-   [Keyman Engine for iPhone and iPad 2.0](/developer/engine/iphone-and-ipad/2.0/)
-   [Keyman Developer](/developer/current-version/)
-   [Keyboard Library](http://keyman.com/developer/keymanweb/keyboards)
-   [Keyman Engine for Android 10.0](/developer/engine/android/current-version/)
