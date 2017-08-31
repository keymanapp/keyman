# Keyman for macOS

## Minimum Mac Requirements
Xcode 8.3.3 (it might also work to use an older version)

## Keyman for macOS Development
Keyman for macOS can be built from a command line (preferred) or Xcode.

### Compiling from Command Line
To build Keyman for macOS, do the following:
1. Open a terminal
2. cd to **keyman/mac**. **build.sh** must be run in the directory containing the script.
3. Build using `./build.sh -no-codesign`. Run `./build.sh -help` to see all options.

### Running Keyman
1. Deploy Keyman locally using `./build.sh -deploy local -deploy-only`.
    * Alternatively copy **keyman/mac/Keyman4MacIM/build/Debug/Keyman.app** to **~/Library/Input Methods**
2. Follow the installation instructions at [Installing Keyman for Mac OS X](https://help.keyman.com/products/mac/1.0/docs/start_download-install_keyman.php).

### Compiling from Xcode
To build using Xcode, you will need to build KeymanEngine4Mac first and then build Keyman4MacIM.

1. Launch Xcode
2. Open **keyman/mac/KeymanEngine4Mac/KeymanEngine4Mac.xcodeproj**
3. Build the project: Product > Build (or Cmd-B)
4. Open **keyman/mac/Keyman4MacIM/Keyman4MacIM.xcodeproj**
5. Open the Project Navigator: View > Navigators > Show Project Navigator (Cmd-1)
6. Select Keyman4MacIM. Click Build Settings, scroll down to the Signing section and change Code Signing Identity to
    Don't Code Sign.
    * This will modify **Keyman4MacIM.xcodeproj**. Do not commit the change.
5. Build the project

### Sample Projects
Not yet available on Mac...
