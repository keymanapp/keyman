# Keyman for iOS

## Prerequisites
See [build configuration](../docs/build/index.md) for details on how to configure your build environment.

## Keyman App
The source for our published iOS Keyman app is found within the **keyman/** subdirectory. This project only contains
the views specific to the Keyman app, while the core functionality is found in KeymanEngine.framework.

### Building
To build Keyman for iOS, please run the build.sh build script within this folder against the `:app` target.
You will need to run with the command-line argument `--debug` or establish alternate code-signing permissions to
complete the build for the final app.
  - Thus:  `./build.sh build:engine --debug`

To run the app within a simulator after build.sh is completed,
1. Open **keymanios.xcworkspace** in Xcode.
2. Run the Keyman target.

The Keyman app relies on KeymanEngine.framework, which will automatically be rebuilt by Xcode whenever it is changed.

Each individual component also possesses its own separate build script; this folder's `./build.sh` can be used
to run any and all of them as desired.

For CI builds, note the parallel `./ci.sh` - this is the primary script used to launch builds as part of our
continuous integration processes.  It may be used to set configuration for calls of the primary `./build.sh` and others.

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
To build using command line, run `./build.sh build:engine --debug`.

To build in Xcode,
1. Open **engine/KMEI/KeymanEngine.xcodeproj**.
2. Build the KME-universal target.

The framework will be built to **engine/KMEI/build/(Debug|Release)-universal/KeymanEngine.framework**.

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
