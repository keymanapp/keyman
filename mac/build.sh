#!/bin/sh

# Include our resource functions; they're pretty useful!
. ../resources/shellHelperFunctions.sh

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac


display_usage() {
    echo "usage: build.sh [build options] [targets]"
    echo
    echo "Build options:"
    echo "  -deploy DEST    Deploys result of Keyman4MacIM. DEST options:"
    echo "                  n|none (default) Not deployed."
    echo "                  l|local          $HOME/Library/Input Methods (kills running process if needed)"
    echo "                  p|preprelease    Builds a DMG and download_info file in output\upload."
    echo "  -deploy-only    Suppresses build/clean/test for all targets."
    echo "  -tier TIER      Used with -deploy p to specify tier: alpha (default), beta, or stable."
    echo "  -version #.#.#  Used to specify the build version number, which should be in the"
    echo "                  form Major.Minor.BuildCounter (optional, but expected if deploy preprelease)"
    echo "  -config NAME    NAME is passed to xcodebuild as -configuration parameter. Defaults to Debug, unless"
    echo "                  the -deploy option is used, in which configuration will be Release (i.e., -config option"
    echo"                   is ignored)."
    echo "  -clean          Removes all previously-existing build products for anything to be built before building."
    echo "  -test           Runs unit tests (not applicable to 'testapp' target)"
    echo "  -no-codesign    Disables code-signing for Keyman4MacIM, allowing it to be performed separately later"
    echo "  -quiet          Do not display any output except for warnings and errors."
    echo
    echo "Targets (to be built and, optionally, cleaned and tested):"
    echo "  engine          KeymanEngine4Mac (engine)"
    echo "  im              Keyman4MacIM (input method)."
    echo "  testapp         Keyman4Mac (test harness)"
    echo "  If no targets are specified, the default targets are 'engine' and 'im'."
    echo
    echo "For deployment, even locally, the app must be code-signed and notarized by Apple. This requires a valid code"
    echo "certificate and an active Appstoreconnect Apple ID. These must be supplied in environment variables:"
    echo
    echo "  * CERTIFICATE_ID: Specifies a certificate from your keychain (e.g. use sha1 or name of certificate)"
    echo "          Use with -no-codesign if you don't have access to the core developer certificates."
    echo "          Core development team members should not need to specify this as the certificate reference is"
    echo "          already configured in the project."
    echo "  * APPSTORECONNECT_PROVIDER: The shortname of the preferred provider in your Apple Developer account"
    echo "          To find this, run:"
    echo "          /Applications/Xcode.app/Contents/Applications/Application\ Loader.app/Contents/itms/bin/iTMSTransporter \\"
    echo "            -m provider -u 'USERNAME' -p 'PASSWORD' -account_type itunes_connect -v off"
    echo "  * APPSTORECONNECT_USERNAME: Your Apple ID login name."
    echo "  * APPSTORECONNECT_PASSWORD: Your Apple ID password (may need to be a app-specific password)."
    exit 1
}

### DEFINE HELPER FUNCTIONS ###

KEYMAN_MAC_BASE_PATH="${BASH_SOURCE[0]}";
if ([ -h "${KEYMAN_MAC_BASE_PATH}" ]) then
	while([ -h "${KEYMAN_MAC_BASE_PATH}" ]) do KEYMAN_MAC_BASE_PATH=`readlink "${KEYMAN_MAC_BASE_PATH}"`; done
fi
pushd . > /dev/null
cd `dirname ${KEYMAN_MAC_BASE_PATH}` > /dev/null
KEYMAN_MAC_BASE_PATH=`pwd`;
popd  > /dev/null

assertOptionsPrecedeTargets() {
    if [[ "$1" =~ ^\- ]]; then
        if $PROCESSING_TARGETS ; then
            fail "Options must be specified before build targets"
        fi
    elif ! $PROCESSING_TARGETS ; then
        PROCESSING_TARGETS=true
        # Since caller is specifying targets explicitly, turn them all off.
        displayInfo "Processing explicit command-line targets..."
        DO_KEYMANENGINE=false
        DO_KEYMANIM=false
    fi
}

do_clean ( ) {
  echo_heading "Cleaning source (Carthage)"
#  rm -rf $KME4M_BUILD_PATH
#  rm -rf $APP_BUILD_PATH
  rm -rf $KEYMAN_MAC_BASE_PATH/Carthage
}

### SET PATHS ###

ENGINE_NAME="KeymanEngine4Mac"
TESTAPP_NAME="Keyman4Mac"
IM_NAME="Keyman4MacIM"
XCODE_PROJ_EXT=".xcodeproj"
PRODUCT_NAME="Keyman"

KME4M_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$ENGINE_NAME"
KMTESTAPP_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$TESTAPP_NAME"
KM4MIM_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$IM_NAME"

KME4M_PROJECT_PATH="$KME4M_BASE_PATH/$ENGINE_NAME$XCODE_PROJ_EXT"
KMTESTAPP_PROJECT_PATH="$KMTESTAPP_BASE_PATH/$TESTAPP_NAME$XCODE_PROJ_EXT"
KMIM_WORKSPACE_PATH="$KM4MIM_BASE_PATH/$IM_NAME.xcworkspace"

# Hard-coded keys. These are safe to distribute.

FABRIC_API_KEY_KEYMAN_DEBUG=68056571ada44ad2d93864380272808ada308b24

# KME4M_BUILD_PATH=engine/KME4M/build
# APP_RESOURCES=keyman/Keyman/Keyman/libKeyman
# APP_BUNDLE_PATH=$APP_RESOURCES/Keyman.bundle
# APP_BUILD_PATH=keyman/Keyman/build/


KME4M_OUTPUT_FOLDER=$KME4M_BUILD_PATH/libKeyman

### PROCESS COMMAND-LINE ARGUMENTS ###

# Default is debug build of Engine and (code-signed) Input Method
PROCESSING_TARGETS=false
CONFIG="Debug"
LOCALDEPLOY=false
PREPRELEASE=false
KM_TIER="alpha"
KM_VERSION=`cat ../resources/VERSION.md`.0
UPDATE_VERSION_IN_PLIST=false
DO_KEYMANENGINE=true
DO_KEYMANIM=true
DO_KEYMANTESTAPP=false
CODESIGNING_SUPPRESSION=""
BUILD_OPTIONS=""
BUILD_ACTIONS="build"
TEST_ACTION=""
CLEAN=false
QUIET=false
SKIP_BUILD=false

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
    key="$1"
    assertOptionsPrecedeTargets "$key"
    case $key in
        -deploy)
            # Deployment requires hardening which only happens in Release configuration;
            # the deployed version cannot be run in the debugger.
            if [[ "$2" =~ ^(l(ocal)?)$ ]]; then
                LOCALDEPLOY=true
                CONFIG="Release"
            elif [[ "$2" =~ ^(p(rep(release)?)?)$ ]]; then
                PREPRELEASE=true
                CONFIG="Release"
            elif ! [[ "$2" =~ ^(n(one)?)$ ]]; then
                fail "Invalid deploy option. Must be 'none', 'local' or 'preprelease'."
            fi
            shift # past argument
            ;;
        -deploy-only)
            SKIP_BUILD=true
            shift # past argument
            ;;
        -tier)
            if [[ "$2" == "" || "$2" =~ ^\- ]]; then
                warn "Missing tier name on command line. Using '$KM_TIER' as default..."
            else
                if [[ "$2" =~ ^(a(lpha)?)$ ]]; then
                    KM_TIER="alpha"
                elif [[ "$2" =~ ^(b(eta)?)$ ]]; then
                    KM_TIER="beta"
                elif [[ "$2" =~ ^(s(table)?)$ ]]; then
                    KM_TIER="stable"
                else
                	KM_TIER=$2
                    fail "Unexpected tier: '$KM_TIER'"
                fi
                shift # past argument
            fi
            ;;
        -version)
            assertValidVersionNbr "$2"
            KM_VERSION="$2"
            UPDATE_VERSION_IN_PLIST=true
            shift # past argument
            ;;
        -config)
            if [[ "$2" == "" || "$2" =~ ^\- ]]; then
                warn "Missing config name on command line. Using 'Debug' as default..."
            else
                if $PREPRELEASE && [[ "$2" != "Release" ]]; then
                    echo "Deployment option 'preprelease' supersedes $2 configuration."
                else
                	if [[ "$2" == "r" || "$2" == "R" ]]; then
                    	CONFIG="Release"
                    elif [[ "$2" == "d" || "$2" == "D" ]]; then
                    	CONFIG="Debug"
                    else
                    	CONFIG="$2"
                    fi
                fi
                shift # past argument
            fi
            ;;
        -clean)
            CLEAN=true
            BUILD_ACTIONS="clean $BUILD_ACTIONS"
            ;;
        -test)
            TEST_ACTION="test"
            ;;
        -no-codesign)
            CODESIGNING_SUPPRESSION="CODE_SIGN_IDENTITY=\"\" CODE_SIGNING_REQUIRED=NO"
            ;;
        -quiet)
            QUIET_FLAG=$1
            BUILD_OPTIONS="$BUILD_OPTIONS $QUIET_FLAG"
            QUIET=true
            ;;
        -h|-?|-help|--help)
            display_usage
            ;;
        engine)
            DO_KEYMANENGINE=true
            ;;
        im)
            DO_KEYMANIM=true
#             if [[ -f $KME4M_BUILD_PATH/ ]]; then
#                 DO_KEYMANENGINE=true
#             fi
            ;;
        testapp)
            DO_KEYMANTESTAPP=true
            ;;
		*)
		    if $PROCESSING_TARGETS ; then
			    fail "Unexpected target: $1. Run with --help for help."
			else
			    fail "Unexpected option: $1. Run with --help for help."
			fi
            ;;
    esac
    shift # past argument
done
if $SKIP_BUILD ; then
    DO_KEYMANENGINE=false
    DO_KEYMANIM=false
    DO_KEYMANTESTAPP=false
    BUILD_ACTIONS=""
    TEST_ACTION=""
    BUILD_OPTIONS=""
    CODESIGNING_SUPPRESSION=""
fi

BUILD_OPTIONS="-configuration $CONFIG $BUILD_OPTIONS"

displayInfo "" \
    "KM_VERSION: $KM_VERSION" \
    "KM_TIER: $KM_TIER" \
    "LOCALDEPLOY: $LOCALDEPLOY" \
    "PREPRELEASE: $PREPRELEASE" \
    "CLEAN: $CLEAN" \
    "DO_KEYMANENGINE: $DO_KEYMANENGINE" \
    "DO_KEYMANIM: $DO_KEYMANIM" \
    "DO_KEYMANTESTAPP: $DO_KEYMANTESTAPP" \
    "CODESIGNING_SUPPRESSION: $CODESIGNING_SUPPRESSION" \
    "BUILD_OPTIONS: $BUILD_OPTIONS" \
    "BUILD_ACTIONS: $BUILD_ACTIONS" \
    "TEST_ACTION: $TEST_ACTION" \
    ""

### Validate notarization environment variables ###

if $LOCALDEPLOY || $PREPRELEASE ; then
  if [ "${CODESIGNING_SUPPRESSION}" != "" ] && [ -z "${CERTIFICATE_ID}" ]; then
    fail "Code signing must be configured for deployment. See build.sh -help for details."
  fi

  if [ -z "${APPSTORECONNECT_PROVIDER}" ] || [ -z "${APPSTORECONNECT_USERNAME}" ] || [ -z "${APPSTORECONNECT_PASSWORD}" ]; then
    fail "Appstoreconnect Apple ID credentials must be configured in environment. See build.sh -help for details."
  fi
fi

### START OF THE BUILD ###

if $CLEAN ; then
    do_clean
fi

if [ "$TEST_ACTION" == "test" ]; then
	carthage bootstrap
fi

execBuildCommand() {
    typeset component="$1"
    shift
    typeset cmnd="$*"
    typeset ret_code

    displayInfo "Building $component:" "$cmnd"
    eval $cmnd
    ret_code=$?
    if [ $ret_code != 0 ]; then
        fail "Build of $component failed! Error: [$ret_code] when executing command: '$cmnd'"
    fi
}

updatePlist() {
	if $UPDATE_VERSION_IN_PLIST ; then
	    KM_COMPONENT_BASE_PATH="$1"
	    KM_COMPONENT_NAME="$2"
		KM_PLIST="$KM_COMPONENT_BASE_PATH/$KM_COMPONENT_NAME/Info.plist"
		if [ -f "$KM_PLIST" ]; then
			echo "Setting $KM_COMPONENT_NAME version to $KM_VERSION in $KM_PLIST"
			/usr/libexec/Plistbuddy -c "Set CFBundleVersion $KM_VERSION" "$KM_PLIST"
			/usr/libexec/Plistbuddy -c "Set CFBundleShortVersionString $KM_VERSION" "$KM_PLIST"
			if [[ "$CONFIG" == "Release" && "$KM_COMPONENT_NAME" == "$IM_NAME" ]]; then
				echo "Setting Fabric APIKey for release build in $KM_PLIST"
				if [ "$FABRIC_API_KEY_KEYMAN4MACIM" == "" ]; then
				    fail "FABRIC_API_KEY_KEYMAN4MACIM environment variable not set!"
				fi
				/usr/libexec/Plistbuddy -c "Set Fabric:APIKey $FABRIC_API_KEY_KEYMAN4MACIM" "$KM_PLIST"
			fi
		else
			fail "File not found: $KM_PLIST"
		fi
	fi
}

### Build Keyman Engine (kmx processor) ###

if $DO_KEYMANENGINE ; then
    echo_heading "Building Keyman Engine"
    updatePlist "$KME4M_BASE_PATH" "$ENGINE_NAME"
    execBuildCommand $ENGINE_NAME "xcodebuild -project \"$KME4M_PROJECT_PATH\" $BUILD_OPTIONS $BUILD_ACTIONS $TEST_ACTION -scheme $ENGINE_NAME"
    execBuildCommand "$ENGINE_NAME dSYM file" "dsymutil \"$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework/Versions/A/$ENGINE_NAME\" -o \"$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework.dSYM\""
fi

### Build Keyman.app (Input Method and Configuration app) ###

if $DO_KEYMANIM ; then
    echo_heading "Building Keyman.app"
	cd "$KM4MIM_BASE_PATH"
    pod update
	pod install
	cd "$KEYMAN_MAC_BASE_PATH"
    updatePlist "$KM4MIM_BASE_PATH" "$IM_NAME"
    execBuildCommand $IM_NAME "xcodebuild -workspace \"$KMIM_WORKSPACE_PATH\" $CODESIGNING_SUPPRESSION $BUILD_OPTIONS $BUILD_ACTIONS -scheme Keyman SYMROOT=\"$KM4MIM_BASE_PATH/build\""
    if [ "$TEST_ACTION" == "test" ]; then
    	if [ "$CONFIG" == "Debug" ]; then
    		execBuildCommand "$IM_NAME tests" "xcodebuild $TEST_ACTION -workspace \"$KMIM_WORKSPACE_PATH\" $CODESIGNING_SUPPRESSION $BUILD_OPTIONS -scheme Keyman SYMROOT=\"$KM4MIM_BASE_PATH/build\""
    	fi
    fi

    # Upload symbols (this was hanging when called from xcodebuild)
    # Actually, it's probably better from here than in the project as we don't need
    # to upload symbols when developing within xcode, only when doing a real build
    # See https://stackoverflow.com/questions/53488083/why-is-fabric-upload-symbols-hanging-on-step-begin-processing-dsym-under-xcode

    cd "$KM4MIM_BASE_PATH"
    if [ "${CONFIGURATION}" = "Release" ]; then
        if [ "${FABRIC_API_KEY_KEYMAN4MACIM}" != "" ]; then
            Pods/Fabric/upload-symbols -a ${FABRIC_API_KEY_KEYMAN4MACIM} -p mac build/${CONFIGURATION}
        fi
    else
        Pods/Fabric/upload-symbols -a ${FABRIC_API_KEY_KEYMAN_DEBUG} -p mac build/${CONFIGURATION}
    fi
    cd "$KEYMAN_MAC_BASE_PATH"
fi

### Build test app ###

if $DO_KEYMANTESTAPP ; then
    echo_heading "Building test app"
    updatePlist "$KMTESTAPP_BASE_PATH" "$TESTAPP_NAME"
    execBuildCommand $TESTAPP_NAME "xcodebuild -project \"$KMTESTAPP_PROJECT_PATH\" $BUILD_OPTIONS $BUILD_ACTIONS"
fi

### Notarize the app for localdeploy and preprelease ###

if $LOCALDEPLOY || $PREPRELEASE ; then
  echo_heading "Notarizing app"
  if [ "${CODESIGNING_SUPPRESSION}" != "" ] && [ -z "${CERTIFICATE_ID}" ]; then
    fail "Notarization and signed executable is required for deployment, even locally. Specify CERTIFICATE_ID environment variable for custom certificate."
  else
    TARGET_PATH="$KM4MIM_BASE_PATH/build/$CONFIG"
    TARGET_APP_PATH="$TARGET_PATH/$PRODUCT_NAME.app"
    TARGET_ZIP_PATH="$TARGET_PATH/$PRODUCT_NAME.zip"
    ALTOOL_LOG_PATH="$TARGET_PATH/altool.log"

    # Note: get-task-allow entitlement must be *off* in our release build (to do this, don't include base entitlements in project build settings)

    # We may need to re-run the code signing if a custom certificate has been passed in
    if [ ! -z "${CERTIFICATE_ID}" ]; then
      echo_heading "Signing with custom certificate (CERTIFICATE_ID environment variable)."
      codesign --force --options runtime --entitlements Keyman4MacIM/Keyman.entitlements --deep --sign "${CERTIFICATE_ID}" "$TARGET_APP_PATH"
    fi

    echo_heading "Zipping Keyman.app for notarization to $TARGET_ZIP_PATH"

    /usr/bin/ditto -c -k --keepParent "$TARGET_APP_PATH" "$TARGET_ZIP_PATH"

    echo_heading "Uploading Keyman.zip to Apple for notarization"

    xcrun altool --notarize-app --primary-bundle-id "com.Keyman.im.zip" --asc-provider "$APPSTORECONNECT_PROVIDER" --username "$APPSTORECONNECT_USERNAME" --password @env:APPSTORECONNECT_PASSWORD --file "$TARGET_ZIP_PATH" --output-format xml > $ALTOOL_LOG_PATH || fail "altool failed"
    cat "$ALTOOL_LOG_PATH"

    ALTOOL_UUID=$(/usr/libexec/PlistBuddy -c "Print notarization-upload:RequestUUID" "$ALTOOL_LOG_PATH")
    ALTOOL_FINISHED=0

    while [ $ALTOOL_FINISHED -eq 0 ]
    do
      # We'll sleep 30 seconds before checking status, to give the altool server time to process the archive
      echo "Waiting 30 seconds for status"
      sleep 30
      xcrun altool --notarization-info "$ALTOOL_UUID" --username "$APPSTORECONNECT_USERNAME" --password @env:APPSTORECONNECT_PASSWORD --output-format xml > "$ALTOOL_LOG_PATH" || fail "altool failed"
      ALTOOL_STATUS=$(/usr/libexec/PlistBuddy -c "Print notarization-info:Status" "$ALTOOL_LOG_PATH")
      if [ "$ALTOOL_STATUS" == "success" ]; then
        ALTOOL_FINISHED=1
      elif [ "$ALTOOL_STATUS" != "in progress" ]; then
        # Probably failing with 'invalid'
        cat "$ALTOOL_LOG_PATH"
        ALTOOL_LOG_URL=$(/usr/libexec/PlistBuddy -c "Print notarization-info:LogFileURL" "$ALTOOL_LOG_PATH")
        curl "$ALTOOL_LOG_URL"
        fail "Notarization failed with $ALTOOL_STATUS; check log at $ALTOOL_LOG_PATH"
      fi
    done

    echo_heading "Notarization completed successfully. Review logs below for any warnings."
    cat "$ALTOOL_LOG_PATH"
    ALTOOL_LOG_URL=$(/usr/libexec/PlistBuddy -c "Print notarization-info:LogFileURL" "$ALTOOL_LOG_PATH")
    curl "$ALTOOL_LOG_URL"
    echo
    echo_heading "Attempting to staple notarization to Keyman.app"
    xcrun stapler staple "$TARGET_APP_PATH" || fail "stapler failed"
  fi
fi

### Deploy as requested ###

if $LOCALDEPLOY ; then
    echo_heading "Attempting local deployment with command:"
    KM4MIM_APP_BASE_PATH="$KM4MIM_BASE_PATH/build/$CONFIG"
    displayInfo "$KM4MIM_BASE_PATH/localdeploy.sh \"$KM4MIM_APP_BASE_PATH\""
    eval "$KM4MIM_BASE_PATH/localdeploy.sh" "$KM4MIM_APP_BASE_PATH"
    if [ $? == 0 ]; then
        displayInfo "Local deployment succeeded!" ""
    else
        fail "Local deployment failed!"
    fi
elif $PREPRELEASE ; then
    echo_heading "Preparing files for release deployment..."
    # Create the disk image
    eval "$KM4MIM_BASE_PATH/make-km-dmg.sh" -version $KM_VERSION $QUIET_FLAG
    if [ $? == 0 ]; then
        displayInfo "Creating disk image succeeded!" ""
    else
        fail "Creating disk image failed!"
    fi

    # Create download info
    eval "$KM4MIM_BASE_PATH/write-download_info.sh" -version $KM_VERSION -tier $KM_TIER
    if [ $? == 0 ]; then
        displayInfo "Writing download_info file succeeded!" ""
    else
        fail "Writing download_info file failed!"
    fi
fi

echo_heading "Build Succeeded!"
exit 0
