#!/bin/sh

# Include our resource functions; they're pretty useful!
. ../resources/shellHelperFunctions.sh

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

display_usage ( ) {
    echo "build.sh [-clean] [-no-kmw] [-only-framework] [-no-codesign] [-no-archive] [-no-build]"
    echo
    echo "  -clean                  Removes all previously-existing build products for KMEI and the Keyman app before building."
    echo "  -no-kmw                 Uses existing keyman.js, doesn't try to build"
    echo "  -only-framework         Builds only the KeymanEngine framework; does not attempt to build the app."
    echo "  -no-carthage            Disables downloading and building for dependencies."
    echo "  -no-codesign            Disables code-signing for the Keyman application, allowing it to be performed separately later."
    echo "                          Will not construct the archive and .ipa.  (includes -no-archive)"
    echo "  -no-archive             Bypasses the archive and .ipa preparation stage."
    echo "  -no-build               Cancels the build entirely.  Useful with 'build.sh -clean -no-build'."
    echo "  -debug                  Sets the configuration to debug mode instead of release."
exit 1
}

do_clean ( ) {
  rm -rf $BUILD_PATH
  rm -rf Carthage
}

### START OF THE BUILD ###

# Base definitions (must be before do_clean call)
DERIVED_DATA=build
BUILD_PATH=$DERIVED_DATA/Build/Products

# Default is building and copying to assets
DO_KMW_BUILD=true
DO_KEYMANAPP=true
DO_ARCHIVE=true
DO_CARTHAGE=true
CLEAN_ONLY=false
CONFIG=Release

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -no-kmw)
            DO_KMW_BUILD=false
            ;;
        -h|-?|-help)
            display_usage
            exit 0
            ;;
        -only-framework)
            DO_KEYMANAPP=false
            ;;
        -no-codesign)
            CODE_SIGN="CODE_SIGN_IDENTITY= CODE_SIGNING_REQUIRED=NO $DEV_TEAM"
            DO_ARCHIVE=false
            ;;
        -no-archive)
            DO_ARCHIVE=false
            ;;
        -clean)
            do_clean
            ;;
        -no-build)
            CLEAN_ONLY=true
            ;;
        -no-carthage)
            DO_CARTHAGE=false
            ;;
        -debug)
            CONFIG=Debug
            ;;
    esac
    shift # past argument
done

# Extended path definitions
KMEI_RESOURCES=engine/KMEI/KeymanEngine/resources
BUNDLE_PATH=$KMEI_RESOURCES/Keyman.bundle/contents/resources
KMW_SOURCE=../web/source

# Build product paths
APP_BUNDLE_PATH=$BUILD_PATH/${CONFIG}-iphoneos/Keyman.app
KEYBOARD_BUNDLE_PATH=$BUILD_PATH/${CONFIG}-iphoneos/SWKeyboard.appex
ARCHIVE_PATH=$BUILD_PATH/${CONFIG}-iphoneos/Keyman.xcarchive
FRAMEWORK_PATH_UNIVERSAL=$BUILD_PATH/$CONFIG-universal/KeymanEngine.framework
FRAMEWORK_PATH_IOS=$BUILD_PATH/$CONFIG-iphoneos/KeymanEngine.framework

XCODEFLAGS="-quiet -configuration $CONFIG"
XCODEFLAGS_EXT="$XCODEFLAGS -derivedDataPath $DERIVED_DATA -workspace keymanios.xcworkspace"

if [ $CLEAN_ONLY = true ]; then
  exit 0
fi

echo
echo "KMW_SOURCE: $KMW_SOURCE"
echo "DO_KMW_BUILD: $DO_KMW_BUILD"
echo "CONFIGURATION: $CONFIG"
echo

update_bundle ( ) {
    if ! [ -d "$BUNDLE_PATH" ]; then
        mkdir -p "$BUNDLE_PATH"
    fi

    if [ $DO_KMW_BUILD = true ]; then
        echo Building KeymanWeb from $KMW_SOURCE
        base_dir="$(pwd)"

        cd $KMW_SOURCE

        if [ "$CONFIG" == "Debug" ]; then
          KMWFLAGS="-debug_embedded"
        else
          KMWFLAGS="-embed"
        fi

        ./build.sh $KMWFLAGS
        if [ $? -ne 0 ]; then
            fail "ERROR:  KeymanWeb's build.sh failed."
        fi

        #Copy over the relevant resources!  It's easiest to do if we navigate to the resulting folder.
        cd ../release/embedded
        cp resources/osk/kmwosk.css        "$base_dir/$BUNDLE_PATH/kmwosk.css"
        cp resources/osk/keymanweb-osk.ttf "$base_dir/$BUNDLE_PATH/keymanweb-osk.ttf"
        cp keyman.js                       "$base_dir/$BUNDLE_PATH/keymanios.js"

        if [ "$CONFIG" == "Debug" ]; then
          cp keyman.js.map                 "$base_dir/$BUNDLE_PATH/keyman.js.map"
        elif [ -f "$base_dir/$BUNDLE_PATH/keyman.js.map" ]; then
          rm                               "$base_dir/$BUNDLE_PATH/keyman.js.map"
        fi

        cd $base_dir
    fi
}

# First things first - update our dependencies.
update_bundle

if [ $DO_CARTHAGE = true ]; then
    echo
    echo "Load dependencies with Carthage"
    carthage bootstrap --platform iOS || fail "carthage boostrap failed"
fi

echo
echo "Building KMEI..."

rm -r $BUILD_PATH/$CONFIG-universal 2>/dev/null
xcodebuild $XCODEFLAGS_EXT $CODE_SIGN -scheme KME-universal

if [ $? -ne 0 ]; then
  fail "KMEI build failed."
fi

assertDirExists "$FRAMEWORK_PATH_UNIVERSAL"

set_version "$FRAMEWORK_PATH_UNIVERSAL" "KeymanEngine"
set_version "$FRAMEWORK_PATH_IOS"

echo "KMEI build complete."

if [ $DO_KEYMANAPP = true ]; then
    # Provides a needed link for codesigning for our CI.
    if ! [ -z "${DEVELOPMENT_TEAM}" ]; then
      DEV_TEAM="DEVELOPMENT_TEAM=${DEVELOPMENT_TEAM}"
    fi

    if [ $DO_ARCHIVE = false ]; then
      xcodebuild $XCODEFLAGS_EXT $CODE_SIGN -scheme Keyman

      if [ $? -ne 0 ]; then
        fail "Keyman app build failed."
      fi

      # Pass the build number information along to the Plist file of the app.
      set_version "$APP_BUNDLE_PATH" "Keyman"
      set_version "$APP_BUNDLE_PATH/Plugins/SWKeyboard.appex"
    else
      # Time to prepare the deployment archive data.
      echo ""
      echo "Preparing .ipa file for deployment."
      xcodebuild $XCODEFLAGS_EXT -scheme Keyman -archivePath $ARCHIVE_PATH archive -allowProvisioningUpdates

      assertDirExists "$ARCHIVE_PATH"

      # Pass the build number information along to the Plist file of the app.
      if [ $BUILD_NUMBER ]; then
        echo "Setting version numbers to $BUILD_NUMBER."
        /usr/libexec/Plistbuddy -c "Set ApplicationProperties:CFBundleVersion $BUILD_NUMBER" "$ARCHIVE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set ApplicationProperties:CFBundleShortVersionString $BUILD_NUMBER" "$ARCHIVE_PATH/Info.plist"

        ARCHIVE_APP="$ARCHIVE_PATH/Products/Applications/Keyman.app"
        ARCHIVE_KBD="$ARCHIVE_APP/Plugins/SWKeyboard.appex"

        set_version "$ARCHIVE_APP" "Keyman"
        set_version "$ARCHIVE_KBD"
      fi

      xcodebuild $XCODEFLAGS -exportArchive -archivePath $ARCHIVE_PATH -exportOptionsPlist exportAppStore.plist -exportPath $BUILD_PATH/${CONFIG}-iphoneos -allowProvisioningUpdates
    fi

    echo ""
    if [ $? = 0 ]; then
        echo "Build succeeded."
    else
        fail "Build failed - please see the log above for details."
    fi
fi
