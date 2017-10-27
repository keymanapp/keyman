#!/bin/sh

# Include our resource functions; they're pretty useful!
. ../resources/shellHelperFunctions.sh

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

display_usage ( ) {
    echo "build.sh [-clean] [-no-kmw] [-libKeyman] [-no-codesign] [-no-archive] [-no-build]"
    echo
    echo "  -clean                  Removes all previously-existing build products for KMEI and the Keyman app before building."
    echo "  -no-kmw                 Uses existing keyman.js, doesn't try to build"
    echo "  -libKeyman              Builds only KMEI for its libKeyman resources; does not attempt to build the app."
    echo "  -no-codesign            Disables code-signing for the Keyman application, allowing it to be performed separately later."
    echo "                          Will not construct the archive and .ipa.  (includes -no-archive)"
    echo "  -no-archive             Bypasses the archive and .ipa preparation stage."
    echo "  -no-build               Cancels the build entirely.  Useful with 'build.sh -clean -no-build'."
    echo "  -debug                  Sets the configuration to debug mode instead of release."
exit 1
}

fetch ( ) {
    name="${2##*/}"  # Extract just the filename.
    echo "Downloading $name"
    rm $2          2> /dev/null
    curl -s $1 -o $2
}

KMEI_RESOURCES=engine/KMEI/KeymanEngine/resources
KMEI_BUILD_PATH=engine/KMEI/build
BUNDLE_PATH=$KMEI_RESOURCES/Keyman.bundle/contents/resources
APP_FRAMEWORK_PATH=keyman/Keyman/KeymanEngine.framework
APP_BUILD_PATH=keyman/Keyman/build/
KMW_SOURCE=../web/source

do_clean ( ) {
  rm -rf $KMEI_BUILD_PATH
  rm -rf $APP_BUILD_PATH
  rm -rf $APP_BUNDLE_PATH
}

### START OF THE BUILD ###

# Default is building and copying to assets
DO_KMW_BUILD=true
DO_KEYMANAPP=true
DO_ARCHIVE=true
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
        -libKeyman)
            DO_KEYMANAPP=false
            ;;
        -no-codesign)
            CODE_SIGN_IDENTITY="CODE_SIGN_IDENTITY="
            CODE_SIGNING_REQUIRED="CODE_SIGNING_REQUIRED=NO"
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
        -debug)
            CONFIG=Debug
            ;;
    esac
    shift # past argument
done

APP_BUNDLE_PATH=$APP_BUILD_PATH/${CONFIG}-iphoneos/Keyman.app
KEYBOARD_BUNDLE_PATH=$APP_BUILD_PATH/${CONFIG}-iphoneos/SWKeyboard.appex
ARCHIVE_PATH=$APP_BUILD_PATH/${CONFIG}-iphoneos/Keyman.xcarchive
BUILD_FRAMEWORK_PATH=$KMEI_BUILD_PATH/$CONFIG-universal/KeymanEngine.framework

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
        echo Building KeymanWeb 10.0 from $KMW_SOURCE
        base_dir="$(pwd)"

        cd $KMW_SOURCE
        ./build.sh -embed
        if [ $? -ne 0 ]; then
            fail "ERROR:  KeymanWeb's build.sh failed."
        fi

        #Copy over the relevant resources!  It's easiest to do if we navigate to the resulting folder.
        cd ../embedded
        cp resources/osk/kmwosk.css        "$base_dir/$BUNDLE_PATH/kmwosk.css"
        cp resources/osk/keymanweb-osk.ttf "$base_dir/$BUNDLE_PATH/keymanweb-osk.ttf"
        cp keyman.js                       "$base_dir/$BUNDLE_PATH/keymanios.js"

        cd $base_dir
    fi
}

# First things first - update our dependencies.
update_bundle

echo
echo "Building KMEI..."

#OTHER_CFLAGS=-fembed-bitcode is relied upon for building the samples by command-line.  They build fine within XCode itself without it, though.

rm -r $KMEI_BUILD_PATH/$CONFIG-universal 2>/dev/null
xcodebuild -quiet -project engine/KMEI/KeymanEngine.xcodeproj -target KME-universal -configuration $CONFIG \
  $CODE_SIGN_IDENTITY $CODE_SIGNING_REQUIRED $DEV_TEAM
assertDirExists "$BUILD_FRAMEWORK_PATH"

if [ $BUILD_NUMBER ]; then
  echo "Setting version number in KeymanEngine to $BUILD_NUMBER."
  /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER" "$BUILD_FRAMEWORK_PATH/Info.plist"
  /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$BUILD_FRAMEWORK_PATH/Info.plist"
fi

rm -r "$APP_FRAMEWORK_PATH"
cp -R "$BUILD_FRAMEWORK_PATH" "$APP_FRAMEWORK_PATH"

echo "KMEI build complete."

if [ $DO_KEYMANAPP = true ]; then
    # Provides a needed link for codesigning for our CI.
    if ! [ -z "${DEVELOPMENT_TEAM}" ]; then
      DEV_TEAM="DEVELOPMENT_TEAM=${DEVELOPMENT_TEAM}"
    fi

    # To dynamically set the parameters in a way xcodebuild can use them, we need to construct the entire xcodebuild call as a string first.
    BUILD_1="xcodebuild -quiet -project keyman/Keyman/Keyman.xcodeproj ${CODE_SIGN_IDENTITY} ${CODE_SIGNING_REQUIRED} ${DEV_TEAM} -target SWKeyboard -configuration ${CONFIG}"
    BUILD_2="xcodebuild -quiet -project keyman/Keyman/Keyman.xcodeproj ${CODE_SIGN_IDENTITY} ${CODE_SIGNING_REQUIRED} ${DEV_TEAM} -target Keyman -configuration ${CONFIG}"

    if [ $DO_ARCHIVE = false ]; then
      # Performs the actual build calls.
      $BUILD_1

      # Pass the build number information along to the Plist file of the keyboard.  We want to intercept it before it's embedded into the app!
      if [ $BUILD_NUMBER ]; then
        echo "Setting version number in SWKeyboard to $BUILD_NUMBER."
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER" "$KEYBOARD_BUNDLE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$KEYBOARD_BUNDLE_PATH/Info.plist"
      fi

      $BUILD_2

      # Pass the build number information along to the Plist file of the app.
      if [ $BUILD_NUMBER ]; then
        echo "Setting version number in Keyman app to $BUILD_NUMBER."
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER" "$APP_BUNDLE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$APP_BUNDLE_PATH/Info.plist"
      fi
    else
      # Time to prepare the deployment archive data.
      echo ""
      echo "Preparing .ipa file for deployment."
      xcodebuild -quiet -workspace keymanios.xcworkspace -scheme Keyman -archivePath $ARCHIVE_PATH archive -configuration $CONFIG -allowProvisioningUpdates

      assertDirExists "$ARCHIVE_PATH"

      # Pass the build number information along to the Plist file of the app.
      if [ $BUILD_NUMBER ]; then
        echo "Setting version numbers to $BUILD_NUMBER."
        /usr/libexec/Plistbuddy -c "Set ApplicationProperties:CFBundleVersion $BUILD_NUMBER" "$ARCHIVE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set ApplicationProperties:CFBundleShortVersionString $BUILD_NUMBER" "$ARCHIVE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER"            "$ARCHIVE_PATH/Products/Applications/Keyman.app/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$ARCHIVE_PATH/Products/Applications/Keyman.app/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER"            "$ARCHIVE_PATH/Products/Applications/Keyman.app/Plugins/SWKeyboard.appex/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$ARCHIVE_PATH/Products/Applications/Keyman.app/Plugins/SWKeyboard.appex/Info.plist"
      fi

      xcodebuild -quiet -exportArchive -archivePath keyman/Keyman/build/${CONFIG}-iphoneos/Keyman.xcarchive -exportOptionsPlist exportAppStore.plist -exportPath keyman/keyman/build/${CONFIG}-iphoneos -configuration $CONFIG  -allowProvisioningUpdates
    fi

    #The resulting archives are placed in the keyman/Keyman/build/Release-iphoneos folder.
    echo ""
    if [ $? = 0 ]; then
        echo "Build succeeded."
    else
        fail "Build failed - please see the log above for details."
    fi
fi
