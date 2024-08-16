#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-help.inc.sh"
. "$KEYMAN_ROOT/mac/mac-utils.inc.sh"

builder_describe "Builds Keyman for macOS." \
  "@/core:mac" \
  "clean" \
  "configure" \
  "build" \
  "publish     Publishes debug info to Sentry and builds DMG, .download_info for distribution" \
  "test" \
  "install     Installs result of Keyman4MacIM locally." \
  ":engine     KeymanEngine4Mac" \
  ":app        Keyman4MacIM" \
  ":mcompile   mnemonic layout recompiler- mac"  
  ":testapp    Keyman4Mac (test harness)" \
  "--quick,-q  Bypasses notarization for $(builder_term install)"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

builder_parse "$@"

# Default is release build of Engine and (code-signed) Input Method
if builder_is_debug_build; then
  CONFIG="Debug"
  CONFIG_TARGET=Pods-Keyman.debug.xcconfig
else
  CONFIG="Release"
  CONFIG_TARGET=Pods-Keyman.release.xcconfig
fi

### SET PATHS ###

ENGINE_NAME="KeymanEngine4Mac"
TESTAPP_NAME="Keyman4Mac"
IM_NAME="Keyman4MacIM"
XCODE_PROJ_EXT=".xcodeproj"
PRODUCT_NAME="Keyman"

KEYMAN_MAC_BASE_PATH="$KEYMAN_ROOT/mac"

KME4M_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$ENGINE_NAME"
KMTESTAPP_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$TESTAPP_NAME"
KM4MIM_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$IM_NAME"

KME4M_PROJECT_PATH="$KME4M_BASE_PATH/$ENGINE_NAME$XCODE_PROJ_EXT"
KMTESTAPP_PROJECT_PATH="$KMTESTAPP_BASE_PATH/$TESTAPP_NAME$XCODE_PROJ_EXT"
KMIM_WORKSPACE_PATH="$KM4MIM_BASE_PATH/$IM_NAME.xcworkspace"

PODS_FOLDER="/mac/Keyman4MacIM/Pods/Target Support Files/Pods-Keyman"

builder_describe_outputs \
  configure     "$PODS_FOLDER/$CONFIG_TARGET" \
  build:engine  "/mac/$ENGINE_NAME/build/$CONFIG" \
  build:app     "/mac/$IM_NAME/build/$CONFIG" \
  build:testapp "/mac/$TESTAPP_NAME/build/$CONFIG"

### DEFINE HELPER FUNCTIONS ###

LOCALDEPLOY=false
PREPRELEASE=false
UPDATE_VERSION_IN_PLIST=true
DO_CODESIGN=true
CODESIGNING_SUPPRESSION="CODE_SIGN_IDENTITY=\"\" CODE_SIGNING_REQUIRED=NO"

QUIET=true

if builder_verbose; then
  BUILD_OPTIONS=""
  QUIET_FLAG=
  QUIET=false
else
  BUILD_OPTIONS="-quiet"
  QUIET_FLAG="-quiet"
fi

BUILD_ACTIONS="build"

SKIP_BUILD=false
UPLOAD_SENTRY=false

# Import local environment variables for build
#
# /mac/localenv.sh can be used to define CERTIFICATE_ID, 
# APPSTORECONNECT_PROVIDER, APPSTORECONNECT_USERNAME, 
# APPSTORECONNECT_PASSWORD, DEVELOPMENT_TEAM variables;
# see /mac/README.md for details.
#
if [[ -f "$THIS_SCRIPT_PATH/localenv.sh" ]]; then
    . "$THIS_SCRIPT_PATH/localenv.sh"
fi

BUILD_OPTIONS="-configuration $CONFIG $BUILD_OPTIONS PRODUCT_VERSION=$VERSION"

### START OF THE BUILD ###

execBuildCommand() {
    declare -r component="$1"
    shift
    declare -r cmnd="$*"

    displayInfo "Building $component:" "$cmnd"
    set +e
    local ret_code=0
    eval "$cmnd" || ret_code=$?
    set -ex

    printXCodeBuildScriptLogs

    if [ $ret_code != 0 ]; then
        builder_die "Build of $component failed! Error: [$ret_code] when executing command: '$cmnd'"
    fi
}

do_clean ( ) {
  rm -rf "$KME4M_BASE_PATH/build"
  rm -rf "$KM4MIM_BASE_PATH/build"
  rm -rf "$KMTESTAPP_BASE_PATH/build"
  rm -rf "$KEYMAN_ROOT/mac/setup/Install Keyman.app"

  builder_heading "Cleaning pods folder (CocoaPods)"
  rm -rf "$PODS_FOLDER"
  builder_heading "Cleaning source (Carthage)"
  rm -rf "$KEYMAN_MAC_BASE_PATH/Carthage"

  # To consider: also mark for xcodebuild to do a clean build?
  # Though, shouldn't the above trigger this anyway?
}

do_configure ( ) {
  carthage bootstrap --use-xcframeworks

  pushd "$KM4MIM_BASE_PATH" > /dev/null
  pod update
  pod install
  popd > /dev/null
}

updatePlist() {
    if $UPDATE_VERSION_IN_PLIST ; then
        KM_PLIST="$1"
        APPNAME="$2"
        if [ ! -f "$KM_PLIST" ]; then
            builder_die "File not found: $KM_PLIST"
        fi
        local YEAR=`date "+%Y"`
        echo "Setting version and related fields to $VERSION_WITH_TAG in $KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set CFBundleVersion $VERSION" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $VERSION" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set :Keyman:SentryEnvironment $VERSION_ENVIRONMENT" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set :Keyman:Tier $TIER" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set :Keyman:VersionTag $VERSION_TAG" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set :Keyman:VersionWithTag $VERSION_WITH_TAG" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set :Keyman:VersionGitTag $VERSION_GIT_TAG" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set :Keyman:VersionRelease $VERSION_RELEASE" "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set CFBundleGetInfoString $APPNAME $VERSION_WITH_TAG for macOS, Copyright © 2017-$YEAR SIL International." "$KM_PLIST"
        /usr/libexec/Plistbuddy -c "Set NSHumanReadableCopyright Copyright © 2017-$YEAR SIL International." "$KM_PLIST"
    fi
}

do_build_engine ( ) {
  ### Build Keyman Engine (kmx, ldml processor) ###

  execBuildCommand $ENGINE_NAME "xcodebuild -project \"$KME4M_PROJECT_PATH\" $BUILD_OPTIONS $BUILD_ACTIONS -scheme $ENGINE_NAME"
  execBuildCommand "$ENGINE_NAME dSYM file" "dsymutil \"$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework/Versions/A/$ENGINE_NAME\" -o \"$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework.dSYM\""
  updatePlist "$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework/Resources/Info.plist" "Keyman Engine"
}

do_build_app ( ) {
  ### Build Keyman.app (Input Method and Configuration app) ###
  builder_heading "Building help"
  build_help_html mac Keyman4MacIM/Keyman4MacIM/Help

  builder_heading "Building Keyman.app"

  execBuildCommand $IM_NAME "xcodebuild -workspace \"$KMIM_WORKSPACE_PATH\" $CODESIGNING_SUPPRESSION $BUILD_OPTIONS $BUILD_ACTIONS -scheme Keyman SYMROOT=\"$KM4MIM_BASE_PATH/build\""
  updatePlist "$KM4MIM_BASE_PATH/build/$CONFIG/Keyman.app/Contents/Info.plist" "Keyman"

  if builder_is_debug_build; then
    ENTITLEMENTS_FILE=Keyman.Debug.entitlements
  else
    ENTITLEMENTS_FILE=Keyman.entitlements

    # We need to re-sign the app after updating the plist file
    execCodeSign eval --force --sign $CERTIFICATE_ID --timestamp --verbose --preserve-metadata=identifier,entitlements "$KM4MIM_BASE_PATH/build/$CONFIG/Keyman.app/Contents/Frameworks/Sentry.framework"

    execCodeSign eval --force --sign $CERTIFICATE_ID --timestamp --verbose --preserve-metadata=identifier,entitlements "$KM4MIM_BASE_PATH/build/$CONFIG/Keyman.app/Contents/Frameworks/KeymanEngine4Mac.framework"

    execCodeSign eval --force --sign $CERTIFICATE_ID --timestamp --verbose -o runtime \
      --entitlements "$KM4MIM_BASE_PATH/$ENTITLEMENTS_FILE" \
      --requirements "'=designated => anchor apple generic and identifier \"\$self.identifier\" and ((cert leaf[field.1.2.840.113635.100.6.1.9] exists) or ( certificate 1[field.1.2.840.113635.100.6.2.6] exists and certificate leaf[field.1.2.840.113635.100.6.1.13] exists and certificate leaf[subject.OU] = \"$DEVELOPMENT_TEAM\" ))'" \
      "$KM4MIM_BASE_PATH/build/$CONFIG/Keyman.app"
  fi
}

do_build_testapp() {
  ### Build test app / harness ###
  execBuildCommand $TESTAPP_NAME "xcodebuild -project \"$KMTESTAPP_PROJECT_PATH\" $BUILD_OPTIONS $BUILD_ACTIONS"
  updatePlist "$KMTESTAPP_BASE_PATH/build/$CONFIG/$TESTAPP_NAME.app/Contents/Info.plist" "Keyman Test App"
}

# In case both install & publish actions are specified, we should still only notarize once.
DID_NOTARIZE=false

do_notarize() {
  if [[ $DID_NOTARIZE == false ]]; then
    ### Validate notarization environment variables ###

    builder_heading "Notarizing app"

    if [ "${CODESIGNING_SUPPRESSION}" != "" ] && [ -z "${CERTIFICATE_ID+x}" ]; then
      builder_die "Notarization and signed executable is required for deployment, even locally. Specify CERTIFICATE_ID environment variable for custom certificate."
    fi

    if [ -z "${APPSTORECONNECT_PROVIDER+x}" ] || [ -z "${APPSTORECONNECT_USERNAME+x}" ] || [ -z "${APPSTORECONNECT_PASSWORD+x}" ]; then
      builder_die "Appstoreconnect Apple ID credentials must be configured in environment. See README.md for details."
    fi

    TARGET_PATH="$KM4MIM_BASE_PATH/build/$CONFIG"
    TARGET_APP_PATH="$TARGET_PATH/$PRODUCT_NAME.app"
    TARGET_ZIP_PATH="$TARGET_PATH/$PRODUCT_NAME.zip"

    # Note: get-task-allow entitlement must be *off* in our release build (to do this, don't include base entitlements in project build settings)

    # We may need to re-run the code signing if a custom certificate has been passed in
    if [ ! -z "${CERTIFICATE_ID+x}" ]; then
      builder_heading "Signing with custom certificate (CERTIFICATE_ID environment variable)."
      execCodeSign direct --force --options runtime --entitlements Keyman4MacIM/Keyman.entitlements --deep --sign "${CERTIFICATE_ID}" "$TARGET_APP_PATH"
    fi

    builder_heading "Zipping Keyman.app for notarization to $TARGET_ZIP_PATH"

    /usr/bin/ditto -c -k --keepParent "$TARGET_APP_PATH" "$TARGET_ZIP_PATH"

    builder_heading "Uploading Keyman.zip to Apple for notarization"
    mac_notarize "$TARGET_PATH" "$TARGET_ZIP_PATH"

    echo
    builder_heading "Attempting to staple notarization to Keyman.app"
    xcrun stapler staple "$TARGET_APP_PATH" || builder_die "stapler failed"

    DID_NOTARIZE=true
  fi
}

do_sentry() {
  # Upload symbols
  builder_heading "Uploading symbols to sentry.keyman.com"

  pushd "$KM4MIM_BASE_PATH" > /dev/null
  sentry-cli upload-dif "build/$CONFIG"
  popd > /dev/null

}

do_install() {
  if ! builder_has_option --quick; then
    do_notarize
  else
    if [ "$(spctl --status)" == "assessments enabled" ]; then
      echo
      builder_warn "WARNING: Notarization is disabled but SecAssessment security policy is still active. Keyman will not run correctly."
      builder_warn "         Disable SecAssessment with 'sudo spctl --master-disable' (or do notarized builds)"
      builder_die  "Re-run without $(builder_term --quick) or disable SecAssessment."
    fi
  fi

  builder_heading "Attempting local deployment with command:"
  KM4MIM_APP_BASE_PATH="$KM4MIM_BASE_PATH/build/$CONFIG"
  displayInfo "$KM4MIM_BASE_PATH/localdeploy.sh \"$KM4MIM_APP_BASE_PATH\""
  "$KM4MIM_BASE_PATH/localdeploy.sh" "$KM4MIM_APP_BASE_PATH"
}

do_publish() {
  do_notarize

  builder_heading "Preparing files for release deployment..."
  ./setup/build.sh

  "$KM4MIM_BASE_PATH/make-km-dmg.sh" $QUIET_FLAG

  # Create download info
  "$KM4MIM_BASE_PATH/write-download_info.sh"

  do_sentry
}

### PROCESS COMMAND-LINE ARGUMENTS ###

builder_run_action clean          do_clean
builder_run_action configure      do_configure

builder_run_action build:engine   do_build_engine
builder_run_action build:app      do_build_app
builder_run_action build:testapp  do_build_testapp

builder_run_action test:engine    execBuildCommand $ENGINE_NAME "xcodebuild -project \"$KME4M_PROJECT_PATH\" $BUILD_OPTIONS test -scheme $ENGINE_NAME"
builder_run_action test:app       execBuildCommand "$IM_NAME-tests" "xcodebuild test -workspace \"$KMIM_WORKSPACE_PATH\" $CODESIGNING_SUPPRESSION $BUILD_OPTIONS -scheme Keyman SYMROOT=\"$KM4MIM_BASE_PATH/build\""

builder_run_action install do_install

builder_run_action publish do_publish