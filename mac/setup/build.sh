#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

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

if [ -f "$SOURCEAPP" ]; then
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
codesign --force --options runtime --deep --sign "${CERTIFICATE_ID}" "$TARGETAPP"

#
# Notarize the app (copied from ../build.sh)
# TODO: merge notarization into function
#

TARGET_ZIP_PATH="$TARGETPATH/Install Keyman.zip"
TARGET_APP_PATH="$TARGETAPP"
ALTOOL_LOG_PATH="$TARGETPATH/altool.log"

builder_heading "Zipping Install Keyman.app for notarization to $TARGET_ZIP_PATH"

/usr/bin/ditto -c -k --keepParent "$TARGET_APP_PATH" "$TARGET_ZIP_PATH"

builder_heading "Uploading Install Keyman.zip to Apple for notarization"

xcrun altool --notarize-app --primary-bundle-id "com.Keyman.install.zip" --asc-provider "$APPSTORECONNECT_PROVIDER" --username "$APPSTORECONNECT_USERNAME" --password @env:APPSTORECONNECT_PASSWORD --file "$TARGET_ZIP_PATH" --output-format xml > $ALTOOL_LOG_PATH || builder_die "altool failed"
cat "$ALTOOL_LOG_PATH"

ALTOOL_UUID=$(/usr/libexec/PlistBuddy -c "Print notarization-upload:RequestUUID" "$ALTOOL_LOG_PATH")
ALTOOL_FINISHED=0

while [ $ALTOOL_FINISHED -eq 0 ]
do
    # We'll sleep 30 seconds before checking status, to give the altool server time to process the archive
    echo "Waiting 30 seconds for status"
    sleep 30
    xcrun altool --notarization-info "$ALTOOL_UUID" --username "$APPSTORECONNECT_USERNAME" --password @env:APPSTORECONNECT_PASSWORD --output-format xml > "$ALTOOL_LOG_PATH" || builder_die "altool failed"
    ALTOOL_STATUS=$(/usr/libexec/PlistBuddy -c "Print notarization-info:Status" "$ALTOOL_LOG_PATH")
    if [ "$ALTOOL_STATUS" == "success" ]; then
    ALTOOL_FINISHED=1
    elif [ "$ALTOOL_STATUS" != "in progress" ]; then
    # Probably failing with 'invalid'
    cat "$ALTOOL_LOG_PATH"
    ALTOOL_LOG_URL=$(/usr/libexec/PlistBuddy -c "Print notarization-info:LogFileURL" "$ALTOOL_LOG_PATH")
    curl "$ALTOOL_LOG_URL"
    builder_die "Notarization failed with $ALTOOL_STATUS; check log at $ALTOOL_LOG_PATH"
    fi
done

builder_heading "Notarization completed successfully. Review logs below for any warnings."
cat "$ALTOOL_LOG_PATH"
ALTOOL_LOG_URL=$(/usr/libexec/PlistBuddy -c "Print notarization-info:LogFileURL" "$ALTOOL_LOG_PATH")
curl "$ALTOOL_LOG_URL"
echo
builder_heading "Attempting to staple notarization to Install Keyman.app"
xcrun stapler staple "$TARGET_APP_PATH" || builder_die "stapler failed"

# Done.
# Now, we can add "Install Keyman.app" to the .dmg for distribution!
