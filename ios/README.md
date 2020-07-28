# Keyman for iOS

## Prerequisites
* Xcode 9.1
* iOS 8+
* SwiftLint (`brew install swiftlint`)
* Carthage (`brew install carthage`)
* [Node.js](https://nodejs.org/) 8.9+ (for building KeymanWeb)
* Coreutils (`brew install coreutils`)
* sentry-cli (`brew install getsentry/tools/sentry-cli`) to utilize Sentry-based error reporting
* jq (`brew install jq`)

## Keyman App

The source for our published iOS Keyman app is found within the **keyman/** subdirectory. This project only contains
the views specific to the Keyman app, while the core functionality is found in KeymanEngine.framework.

### Building
Building Keyman Web is a precursor for compiling KMEI, so verify your system has all the [Minimum Web Compilation Requirements](../web/README.md#minimum-web-compilation-requirements)

To build Keyman for iOS, please run the build.sh build script within this folder.
You will need to run with the command-line argument `-no-codesign` or establish alternate code-signing permissions to
complete the build for the final app.

To run the app within a simulator after build.sh is completed,
1. Open **keymanios.xcworkspace** in Xcode.
2. Run the Keyman target.

The Keyman app relies on KeymanEngine.framework, which will automatically be rebuilt by Xcode whenever it is changed.

### Running the app on a physical device
In order to test any code changes on a physical device, Apple requires that an app be code-signed.  (Note that code-signing is not necessary for Simulator runs.)  The quickest way to handle this for personal testing:

- Open Xcode and the settings for the Keyman target.
- Establish personal code-signing permissions.
  - Go to the **General** tab and enable "Automatically manage signing" and select your "Personal Team."
  - Change the "Bundle Identifier" for both the Keyman and SWKeyboard targets to something personal and unique.
  - Afterward, swap to the **Capabilities** tab, remove the existing "App Groups", and define your own unique group for the Keyman target.
  - Then, make the "App Group" settings of SWKeyboard match those of Keyman.
  - Perform a find and replace for the group identifier "group.KM4I" and with your selected unique group name.
  - Correct any further error messages Xcode presents you with.
  - **Note**:  All changes for this step (code-signing permissions, app groups, bundle identifier, etc) must be reverted to the original settings for submitted PRs.
- Under the **Product > Destination** Xcode menu, select the device you wish to use for testing.
- Attempt to "Run" Keyman on the device.  The operation should fail, but this step is necessary to continue.
- Now, on the device itself, go to **Settings > General > Profiles & Device Management** and give your developer account trusted permissions.

You should then be freely able to test the app on your personal development device.

## KeymanEngine

KeymanEngine is a Swift 4 framework containing the core functionality of Keyman, including
* The keyboard view and text views that use the Keyman keyboard.
* Keyboard selection and downloading.

### Building
To build using command line, run `./build.sh -only-framework`.

To build in Xcode,
1. Open **engine/KMEI/KeymanEngine.xcodeproj**.
2. Build the KME-universal target.

The framework will be built to **engine/KMEI/build/(Debug|Release)-universal/KeymanEngine.framework**.

If it doesn't build, and you have upgraded from Xcode 10.0 (or earlier) to 10.1 (or later), it may not
build due to "Could not find any available simulators for iOS" error from Carthage, probably while
building DeviceKit. Xcode 10.1 changed the output format which confuses Carthage. Upgrade Carthage:
```
brew upgrade carthage
brew link --overwrite carthage
```

### Linking with KeymanEngine
1. Add KeymanEngine.framework to 'Linked Frameworks and Libraries' and 'Embedded Binaries' in your project targets.

### Usage
`TextView` and `TextField` can be used in place of `UITextView` and `UITextField`. These views use Keyman as a keyboard
and will render with custom fonts. `KeyboardPickerButton` and `KeyboardPickerBarButtonItem` open a view for the user to
switch keyboards or download new keyboards. `InputViewController` can be subclassed to create a system keyboard.

`Manager` must be set up when your app is initializing. If your app has a system keyboard, you must create an app group
entitlement, and set `Manager.applicationGroupIdentifier` before any KeymanEngine classes are initialized and before
`Manager.shared` is used. Also set `Manager.shared.openURL` to `UIApplication.shared.openURL` in your app. Refer to
`Manager` for more detailed usage and configuration options.

### Samples
The KeymanEngine project contains a demo app that demonstrates usage of the framework. There are also two sample apps in
**sample/**. KMSample1 demonstrates use of `TextView` and bundling a keyboard in your app. KMSample2 demonstrates how to
create a system keyboard.

To build the samples, `cd` into the project directory and run `./build.sh`.

### Compiling the app's offline help
Extra prerequisite:
* `wget` (`brew install wget`)

The script `build-help.sh` uses the `wget` tool to construct an offline bundle from the current
online version of help on help.keyman.com.  When significant changes to help content have been
made, it is advisable to manually re-run this script to update the app's offline content.