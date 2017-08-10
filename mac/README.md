# Keyman for macOS

## Minimum Mac Requirements
Xcode 8.3.3 (it might also work to use an older version)

## Keyman for macOS Development
Keyman for macOS can be built from a command line (preferred) or Xcode.
Before proceeding to the steps below, make sure you can build the [Keyman Engine for Android](#how-to-build-Keyman-Engine-for-Android) first. That will ensure all the needed SDK files are downloaded.

### Compiling from Command line
To build Keyman for macOS, do the following:
1. Open a Terminal window.
2. Change to please run the build.sh build script within this folder. Run
with the -help switch to see all options.
You will need to either run with the command-line argument -no-codesign or obtain the
necessary security permissions and certificate data as part of our core development team
to complete the build for the final app.
To build using Xcode, you will need to build KeymanEngine4Mac first and then build
Keyman4MacIM.

Keyman4MacIM is an input method, and as such is installed in the Input Methods folder.
The easiest way to install it locally for testing is to specify "-deploy local" on the
command line. See https://help.keyman.com/products/macosx/start_download-install_keyman.php
for complete instructions on setting up Keyman for the first time and installing keyboards.

The Keyman4Mac project builds a “test-bed” app that can be used to test keyboards without
installing the input method. This also serves as a rudimentary example of how the Keyman
engine might be incorporated directly into a custom app, if desired.

1. Launch a command prompt
2. Change to the directory mac subdirectory of the keyman repo directory.
3. Run `./build.sh -help` to see all build options. For example:
   i. To build and run tests on a debug version of the engine and input method, without code-signing:
		`./build.sh -test -no-codesign`
		The input method app will be in Keyman4MacIM/build/Debug
   ii. To build a release version and deploy it as an installed input method on your local machine:
		`./build.sh -deploy local -config release`
		The input method will be deployed in ~/Library/Input Methods/
   iii. To do a clean release build of engine, input method and test app, plus prepare the files that would be deployed to the alpha download site for version 10.2.56:
		`./build.sh -clean -deploy preprelease -version 10.2.56 -tier alpha` engine im testapp
		The input method files for upload will be in Keyman4MacIM/output/upload/10.2.56
		The test app will be in Keyman4Mac/build/Release/

### Compiling from Xcode
1. Launch Xcode and open **keyman/mac/KeymanEngine4Mac/KeymanEngine4Mac.xcodeproj
2. Press Command-B to build.
3. Open **keyman/mac/Keyman4MacIM/Keyman4MacIM.xcodeproj
4. Press Command-B to build.
5. Open **keyman/mac/Keyman4Mac/Keyman4Mac.xcodeproj
6. Press Command-B to build.

### Sample Projects #
Not yet available on Mac...