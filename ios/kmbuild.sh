#!/bin/sh

# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

display_usage ( ) {
    echo "build.sh [-clean] [-no-kmw] [-only-framework] [-no-codesign] [-no-archive] [-no-build] [-upload-sentry]"
    echo
    echo "  -clean                  Removes all previously-existing build products for KMEI and the Keyman app before building."
    echo "  -no-kmw                 Uses existing keyman.js, doesn't try to build"
    echo "  -only-framework         Builds only the KeymanEngine framework; does not attempt to build the app."
    echo "  -no-carthage            Disables downloading and building for dependencies."
    echo "  -no-codesign            Disables code-signing for the Keyman application, allowing it to be performed separately later."
    echo "                          Will not construct the archive and .ipa.  (includes -no-archive)"
    echo "  -no-archive             Bypasses the archive and .ipa preparation stage."
    echo "  -no-build               Cancels the build entirely.  Useful with 'build.sh -clean -no-build'."
    echo "  -upload-sentry          Uploads debug symbols, etc, to Sentry"
    echo "  -debug                  Sets the configuration to debug mode instead of release."
    echo "  -download-resources     Download up-to-date versions of the engine's default resources from downloads.keyman.com."
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
DO_KMP_DOWNLOADS=false
CODE_SIGN=

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
            CODE_SIGN="CODE_SIGN_IDENTITY= CODE_SIGNING_REQUIRED=NO ${DEV_TEAM:-} CODE_SIGN_ENTITLEMENTS= CODE_SIGNING_ALLOWED=NO"
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
            # Overrides default set by build-utils.sh.
            UPLOAD_SENTRY=false
            ;;
        -no-carthage)
            DO_CARTHAGE=false
            ;;
        -upload-sentry)
            # Overrides default set by build-utils.sh.
            UPLOAD_SENTRY=true
            ;;
        -debug)
            CONFIG=Debug
            ;;
        -download-resources)
            DO_KMP_DOWNLOADS=true
            ;;
    esac
    shift # past argument
done

# Extended path definitions
KMEI_RESOURCES=engine/KMEI/KeymanEngine/resources
BUNDLE_PATH=$KMEI_RESOURCES/Keyman.bundle/contents/resources
KMW_SOURCE=../web/source

DEFAULT_KBD_ID="sil_euro_latin"
DEFAULT_LM_ID="nrc.en.mtnt"

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
echo "DO_KMP_DOWNLOADS: $DO_KMP_DOWNLOADS"
echo "CONFIGURATION: $CONFIG"
echo

update_bundle ( ) {
    if ! [ -d "$BUNDLE_PATH" ]; then
        mkdir -p "$BUNDLE_PATH"
    fi

    base_dir="$(pwd)"

    if [ $DO_KMW_BUILD = true ]; then
        echo Building KeymanWeb from $KMW_SOURCE

        cd $KMW_SOURCE

        if [ "$CONFIG" == "Debug" ]; then
          KMWFLAGS="-debug_embedded"
        else
          KMWFLAGS="-embed"
        fi

        # Local development optimization - cross-target Sentry uploading when requested
        # by developer.  As it's not CI, the Web artifacts won't exist otherwise...
        # unless the developer manually runs the correct build configuration accordingly.
        if [[ $VERSION_ENVIRONMENT == "local" ]] && [[ $UPLOAD_SENTRY == true ]]; then
          KMWFLAGS="$KMWFLAGS -upload-sentry"
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

        cd ../../node_modules/@keymanapp/web-sentry-manager/dist/

        cp index.js                        "$base_dir/$BUNDLE_PATH/keyman-sentry.js"

        cd "$base_dir"
    fi

    # Our default resources are part of the bundle, so let's check on them.
    if [ ! -f "$base_dir/$BUNDLE_PATH/$DEFAULT_KBD_ID.kmp" ]; then
      DO_KMP_DOWNLOADS=true
      warn "OVERRIDE:  Performing -download-resources run, as the keyboard package is missing!"
    elif [ ! -f "$base_dir/$BUNDLE_PATH/$DEFAULT_LM_ID.model.kmp" ]; then
      DO_KMP_DOWNLOADS=true
      warn "OVERRIDE:  Performing -download-resources run, as the lexical model package is missing!"
    fi

    if [ $DO_KMP_DOWNLOADS = true ]; then
      echo_heading "Downloading up-to-date packages for default resources"

      downloadKeyboardPackage "$DEFAULT_KBD_ID" "$base_dir/$BUNDLE_PATH/$DEFAULT_KBD_ID.kmp"
      downloadModelPackage "$DEFAULT_LM_ID" "$base_dir/$BUNDLE_PATH/$DEFAULT_LM_ID.model.kmp"

      echo "${SUCCESS_GREEN}Packages successfully updated${NORMAL}"

      # If we aren't downloading resources, make sure copies of them already exist!
    fi
}

# First things first - update our dependencies.
update_bundle

if [ $DO_CARTHAGE = true ]; then
  echo
  echo "Load dependencies with Carthage"

  carthage checkout || fail "Carthage dependency loading failed"

  # Carthage sometimes picks the wrong .xcworkspace if two are available in a dependency's repo.
  # Easiest way to override it - delete the wrong one (or just its scheme)

  # Deleted workspace - a test for proper deployment to CocoaPods.  Doesn't matter here.
  rm -r ./Carthage/Checkouts/DeviceKit/CocoaPodsVerification/ || fail "Carthage dependency loading failed"

  carthage build --use-xcframeworks --platform iOS || fail "Carthage dependency loading failed"
fi

echo
echo "Build products will be set with the following version metadata:"
echo "  * VERSION=$VERSION"
echo "  * VERSION_WITH_TAG=$VERSION_WITH_TAG"
echo "  * VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT"
echo "  * UPLOAD_SENTRY=$UPLOAD_SENTRY"
echo
echo "Building KMEI..."

if [ -d "$BUILD_PATH/$CONFIG-universal" ]; then
  rm -r $BUILD_PATH/$CONFIG-universal
fi

xcodebuild $XCODEFLAGS_EXT $CODE_SIGN -scheme KME-universal \
           VERSION=$VERSION \
           VERSION_WITH_TAG=$VERSION_WITH_TAG \
           VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
           UPLOAD_SENTRY=$UPLOAD_SENTRY

if [ $? -ne 0 ]; then
  fail "KMEI build failed."
fi

assertDirExists "$FRAMEWORK_PATH_UNIVERSAL"

echo "KMEI build complete."

if [ $DO_KEYMANAPP = true ]; then
  echo ""
  echo "Building offline help."
  ./build-help.sh html

  echo ""
  echo "Building Keyman app."

  # Provides a needed link for codesigning for our CI.
  if ! [ -z "${DEVELOPMENT_TEAM+x}" ]; then
    DEV_TEAM="DEVELOPMENT_TEAM=${DEVELOPMENT_TEAM}"
  else
    DEV_TEAM=
  fi

  if [ $DO_ARCHIVE = false ]; then
    xcodebuild $XCODEFLAGS_EXT $CODE_SIGN -scheme Keyman \
                VERSION=$VERSION \
                VERSION_WITH_TAG=$VERSION_WITH_TAG \
                VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
                UPLOAD_SENTRY=$UPLOAD_SENTRY

    if [ $? -ne 0 ]; then
      fail "Keyman app build failed."
    fi

  else
    # Time to prepare the deployment archive data.
    echo ""
    echo "Preparing .ipa file for deployment."
    xcodebuild $XCODEFLAGS_EXT -scheme Keyman \
                -archivePath $ARCHIVE_PATH \
                archive -allowProvisioningUpdates \
                VERSION=$VERSION \
                VERSION_WITH_TAG=$VERSION_WITH_TAG \
                VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
                UPLOAD_SENTRY=$UPLOAD_SENTRY

    assertDirExists "$ARCHIVE_PATH"

    xcodebuild $XCODEFLAGS -exportArchive -archivePath $ARCHIVE_PATH \
                -exportOptionsPlist exportAppStore.plist \
                -exportPath $BUILD_PATH/${CONFIG}-iphoneos -allowProvisioningUpdates
  fi

  echo ""
  if [ $? = 0 ]; then
      echo "Build succeeded."
  else
      fail "Build failed - please see the log above for details."
  fi
fi
