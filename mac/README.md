# Keyman for macOS

## Prerequisites
See [build configuration](../docs/build/index.md) for details on how to configure your build environment.

## Keyman for macOS Development

Keyman for macOS can be built from a command line (preferred) or Xcode.

### Setting up your code signing and notarization environment.

With macOS 10.14 and later, Keyman must be notarized in order to be permitted to interact with
keyboard input. You have two options for local builds:

1. You can disable security checks for the system with the command:

    `sudo spctl --master-disable`

   This has obvious security implications and the risk is up to you. However, builds are
   much, much, faster than with the alternative option below, and for extensive local
   debugging is far less painful.

2. Or, you must sign and notarize every build. See below. (Use `-deploy local`)

### Signing and notarizing builds

Keyman must be signed then notarized by Apple, even for local test builds. This requires additional
configuration for your build environment.

1. First, open XCode, Settings, Accounts, and select Manage Certificates for the identity
   you wish to use for signing. Click **+** and select **Apple Development**. A
   certificate will then be generated and listed in your Keychain.

2. Find the SHA-1 hash. To find the certificate in terminal:

   `security find-certificate -Z -c "<your-apple-id>" -a`

   (If you have more than one, you may need to use Keychain Access to differentiate).
   Copy the SHA-1 hash from this command's output.

   Take note also of the Development Team ID, found in parentheses at the end of the
   `labl` blob line.

2. Determine the Apple ID details in order to run a build. You may wish to create an
   App-Specific Password at https://appleid.apple.com/ and use this. Your Shortname will
   be either your 10-digit Team Identifier or a shortname that can be extracted with
   the following command:

   `/Applications/Xcode.app/Contents/Developer/usr/bin/iTMSTransporter -m provider -u '<Username>' -p '<Password>' -account_type itunes_connect -v off`

   Note: Use your Apple ID for `<Username>` and the app-specific password you generated above
   for `<Password>`. You'll use these in the next step as well.

3. Add the following environment variables to mac/localenv.sh (or to your .bashrc file), replacing with the
   values you collected in the previous steps:

        export CERTIFICATE_ID=<SHA1-Fingerprint>
        export APPSTORECONNECT_PROVIDER=<Shortname>
        export APPSTORECONNECT_USERNAME=<Username>
        export APPSTORECONNECT_PASSWORD=<Password>
        export DEVELOPMENT_TEAM=<TeamID>

### Compiling from Command Line

To build Keyman for macOS, do the following:
1. Open a Terminal window.
2. Build using `mac/build.sh clean configure build`

Note: If Carthage prompts you to allow it access to your github credentials, it's fine to click Deny.

### Running Keyman

1. Deploy Keyman locally using `mac/build.sh install`.
    * This will notarize the app, signing with your local credentials if not already signed, and copy **keyman/mac/Keyman4MacIM/build/Debug/Keyman.app** to **~/Library/Input Methods**
2. If running for the first time, follow the installation instructions at [Install Keyman for macOS].

You can also use `mac/build.sh build install` to do a single-step build, notarize, and deploy (see above for faster options).

### Compiling from Xcode

To build using Xcode, you will need to build KeymanEngine4Mac first and then build Keyman4MacIM. The very _first_ time after getting the source code (and any time the Podfile is edited; e.g., to install new pods), you need to go to **keyman/mac/Keyman4MacIM** and run "pod install" in Terminal.

1. Launch Xcode
2. Open **keyman/mac/KeymanEngine4Mac/KeymanEngine4Mac.xcodeproj**
3. Build the project: Product > Build (or Cmd-B)
4. Open **keyman/mac/Keyman4MacIM/Keyman4MacIM.xcodeproj**
5. If you do not have signing credentials from the core development team, disable code signing in Xcode.
    1. Open the Project Navigator: View > Navigators > Show Project Navigator (Cmd-1)
    2. Select Keyman4MacIM and click Build Settings
    3. In the Signing section, change Code Signing Identity to Don't Code Sign. This will modify
    **Keyman4MacIM.xcodeproj**. Do not commit the change.
6. Build the project. Refer to [Running Keyman](#running-keyman) on how to install the app.

### Testing

The Keyman4Mac project builds a test-bed app that can be used to test keyboards without installing the input method.
It can also be used as reference for the usage of Keyman Engine.

Keyman4Mac tests are run using `./build.sh -test -no-codesign`.

### A note about Homebrew and xcodebuild

If you get this error from xcodebuild:

`Error: xcode-select: error: tool 'xcodebuild' requires Xcode, but active developer directory is a command line tools instance`

Then run this command to fix the build environment:

`sudo xcode-select --switch /Applications/Xcode.app/Contents/Developer`

[Install Keyman for macOS]: https://help.keyman.com/products/mac/current-version/start/install-keyman

### New Mac Config App

This is now in development.
