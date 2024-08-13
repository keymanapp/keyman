#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/mac/mac-utils.inc.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

cd "${THIS_SCRIPT_PATH}"

#
# Constants
#

SOURCEAPP="./Install Keyman.app"
TARGETPATH=../Keyman4MacIM/output
TARGETAPP="$TARGETPATH/Install Keyman.app"
KEYMANAPP=../Keyman4MacIM/build/Release/Keyman.app

#
# Build basic app and copy icons
#

if [ -e "$SOURCEAPP" ]; then
  rm -rf "$SOURCEAPP"
fi

osacompile -o "$SOURCEAPP" -x source/install.applescript
cp source/applet.icns "$SOURCEAPP/Contents/Resources/applet.icns"

# Add arm64 to plist so that the script is not marked as requiring Rosetta
# ... LSArchitecturePriority
# https://developer.apple.com/documentation/apple-silicon/building-a-universal-macos-binary#Specify-the-Launch-Behavior-of-Your-App
/usr/libexec/PlistBuddy -c "Add :LSArchitecturePriority array" "$SOURCEAPP/Contents/Info.plist"
/usr/libexec/PlistBuddy -c "Add :LSArchitecturePriority:0 string 'arm64'" "$SOURCEAPP/Contents/Info.plist"
/usr/libexec/PlistBuddy -c "Add :LSArchitecturePriority:1 string 'x86_64'" "$SOURCEAPP/Contents/Info.plist"

#
# Build textinputsource
#

pushd textinputsource
./build.sh
popd

#
# Build install candidate of "Install Keyman"
#

# TODO: get script folder so we can run this from anywhere safely
# TODO: set version number in install image

# Remove old install image
mkdir -p "$TARGETPATH"
rm -rf "$TARGETAPP" || true

# Copy new install image to output folder
/bin/cp -R "$SOURCEAPP" "$TARGETPATH/"

# Copy Keyman.app into install image
/bin/cp -R "$KEYMANAPP" "$TARGETAPP/Contents/MacOS/Keyman.app"

# Copy textinputsource into install image
cp textinputsource/textinputsource "$TARGETAPP/Contents/MacOS/textinputsource"

# NOW TO notarize...
xattr -rc "$TARGETAPP"
execCodeSign direct --force --options runtime --deep --sign "${CERTIFICATE_ID}" "$TARGETAPP"

#
# Notarize the app (copied from ../build.sh)
# TODO: merge notarization into function
#

TARGET_ZIP_PATH="$TARGETPATH/Install Keyman.zip"
TARGET_APP_PATH="$TARGETAPP"

builder_heading "Zipping Install Keyman.app for notarization to $TARGET_ZIP_PATH"

/usr/bin/ditto -c -k --keepParent "$TARGET_APP_PATH" "$TARGET_ZIP_PATH"

builder_heading "Uploading Install Keyman.zip to Apple for notarization"
mac_notarize "$TARGETPATH" "$TARGET_ZIP_PATH"

builder_heading "Attempting to staple notarization to Install Keyman.app"
xcrun stapler staple "$TARGET_APP_PATH" || builder_die "stapler failed"

# Done.
# Now, we can add "Install Keyman.app" to the .dmg for distribution!
