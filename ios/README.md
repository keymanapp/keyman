# Keyman for iOS

## Prerequisites
* Xcode 9
* iOS 8+
* SwiftLint (`brew install swiftlint`)

### Compiling from Command line
To build Keyman for iOS, please run the build.sh build script within this folder.
You will need to run with the command-line argument `-no-codesign` or establish alternate code-signing permissions to complete the build for the final app.

To run the app within a simulator after build.sh is completed, "Run" the Keyman scheme/target within Xcode.

### Running the app on a physical device
In order to test any code changes on a physical device, Apple requires that an app be code-signed.  The quickest way to handle this for personal testing:

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
