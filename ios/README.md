# Keyman for iOS

## Minimum iOS Requirements
Xcode

### Compiling from Command line
To build Keyman for iOS, please run the build.sh build script within this folder.
You will need to either run with the command-line argument `-no-codesign` or obtain the necessary security permissions and certificate data as part of our core development team to complete the build for the final app.

To run the app within a simulator after build.sh is completed, "Run" the Keyman scheme/target within Xcode.

### Running the app on a physical device
In order to test any code changes on a physical device, Apple requires that an app be code-signed.  The quickest way to handle this for personal testing:

- Open Xcode and the settings for the Keyman target (under the Keyman project).
- Establish personal code-signing permissions
  - Go to the **General** tab and enable "Automatically manage signing" and select your "Personal Team."
	- Afterward, swap to the **Capabilities** tab and *disable* "App Groups"
	- Correct any further error messages Xcode presents you with.
	- **Note**:  Code-signing permissions must be reverted to the original settings for PRs.
- Under the **Product > Destination** Xcode menu, select the device you wish to use for testing.
- Attempt to "Run" Keyman on the device.  The operation should fail, but this step is necessary to continue.
- Now, on the device itself, go to **Settings > General > Profiles & Device Management** and give your developer account trusted permissions.

You should then be freely able to test the app on your personal development device.
