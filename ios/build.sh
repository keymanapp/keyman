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
APP_RESOURCES=keyman/Keyman/Keyman/libKeyman
APP_BUNDLE_PATH=$APP_RESOURCES/Keyman.bundle
APP_BUILD_PATH=keyman/Keyman/build/
KMW_SOURCE=../web/source

do_clean ( ) {
  rm -rf $KMEI_BUILD_PATH
  rm -rf $APP_BUILD_PATH
  rm -rf $APP_BUNDLE_PATH
}

KMEI_OUTPUT_FOLDER=$KMEI_BUILD_PATH/libKeyman

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

    #Copy the updated bundle to our output folder.
    cp -Rf $KMEI_RESOURCES/Keyman.bundle ${KMEI_OUTPUT_FOLDER}
}

# First things first - update our dependencies.

if ! [ -d "${KMEI_OUTPUT_FOLDER}" ]; then
    mkdir -p "${KMEI_OUTPUT_FOLDER}"
fi

update_bundle

echo
echo "Building KMEI..."

#OTHER_CFLAGS=-fembed-bitcode is relied upon for building the samples by command-line.  They build fine within XCode itself without it, though.

rm -r $KMEI_BUILD_PATH/${CONFIG}-iphoneos 2>/dev/null
xcodebuild -quiet -project engine/KMEI/KeymanEngine.xcodeproj -target KME-iphoneos OTHER_CFLAGS=-fembed-bitcode \
  -configuration $CONFIG
assertFileExists $KMEI_BUILD_PATH/${CONFIG}-iphoneos/libKME-iphoneos.a

rm -r $KMEI_BUILD_PATH/${CONFIG}-iphonesimulator 2>/dev/null
xcodebuild -quiet -project engine/KMEI/KeymanEngine.xcodeproj -sdk iphonesimulator PLATFORM_NAME=iphonesimulator \
  -target KME-iphonesimulator OTHER_CFLAGS=-fembed-bitcode -configuration $CONFIG
assertFileExists $KMEI_BUILD_PATH/${CONFIG}-iphonesimulator/libKME-iphonesimulator.a

# Combine the two builds into KMEI.
rm -f ${KMEI_OUTPUT_FOLDER}/libKeyman.a  2>/dev/null

lipo -create "$KMEI_BUILD_PATH/${CONFIG}-iphonesimulator/libKME-iphonesimulator.a" "$KMEI_BUILD_PATH/${CONFIG}-iphoneos/libKME-iphoneos.a" -output "${KMEI_OUTPUT_FOLDER}/libKeyman.a"

assertFileExists $KMEI_OUTPUT_FOLDER/libKeyman.a
cp -Rf "$KMEI_BUILD_PATH/${CONFIG}-iphoneos/usr/local/include" "${KMEI_OUTPUT_FOLDER}/"

echo "KMEI build complete."

if [ $DO_KEYMANAPP = true ]; then
    # Copy the KMEI resources into the Keyman App's project.
    if ! [ -d "${APP_RESOURCES}" ]; then
        mkdir -p "${APP_RESOURCES}"
    fi
    cp -Rf $KMEI_OUTPUT_FOLDER/* $APP_RESOURCES

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
        echo "Setting version numbers to $BUILD_NUMBER."
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER" "$KEYBOARD_BUNDLE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$KEYBOARD_BUNDLE_PATH/Info.plist"
      fi

      $BUILD_2

      # Pass the build number information along to the Plist file of the app.
      if [ $BUILD_NUMBER ]; then
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $BUILD_NUMBER" "$APP_BUNDLE_PATH/Info.plist"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $BUILD_NUMBER" "$APP_BUNDLE_PATH/Info.plist"
      fi
    else
      # Time to prepare the deployment archive data.
      echo ""
      echo "Preparing .ipa file for deployment."
      xcodebuild -quiet -workspace keymanios.xcworkspace -scheme Keyman -archivePath $ARCHIVE_PATH archive -configuration $CONFIG

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

      xcodebuild -quiet -exportArchive -archivePath keyman/Keyman/build/${CONFIG}-iphoneos/Keyman.xcarchive -exportOptionsPlist exportAppStore.plist -exportPath keyman/keyman/build/${CONFIG}-iphoneos -configuration $CONFIG
    fi

    #The resulting archives are placed in the keyman/Keyman/build/Release-iphoneos folder.
    echo ""
    if [ $? = 0 ]; then
        echo "Build succeeded."
    else
        fail "Build failed - please see the log above for details."
    fi
fi
