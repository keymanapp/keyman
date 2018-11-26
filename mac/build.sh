#!/bin/sh

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" != "darwin"* ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

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
    echo "                  the deploy option is used to specify preprelease, in which configuration will be"
    echo "                  Release (i.e., -config option is ignored)."
    echo "  -clean          Removes all previously-existing build products for anything to be built before building."
    echo "  -test           Runs unit tests (not applicable to 'testapp' target)"
    echo "  -no-codesign    Disables code-signing for Keyman4MacIM, allowing it to be performed separately later"
    echo "                  (ignored if a deploy option other than 'none' is specified)."
    echo "  -quiet          Do not display any output except for warnings and errors."
    echo
    echo "Targets (to be built and, optionally, cleaned and tested):"
    echo "  engine          KeymanEngine4Mac (engine)"
    echo "  im              Keyman4MacIM (input method)."
    echo "  testapp         Keyman4Mac (test harness)"
    echo "  If no targets are specified, the default targets are 'engine' and 'im'."
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

. $KEYMAN_MAC_BASE_PATH/bashHelperFunctions.sh

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
#  rm -rf $KME4M_BUILD_PATH
#  rm -rf $APP_BUILD_PATH
  rm -rf $KEYMAN_MAC_BASE_PATH/Carthage
}

### SET PATHS ###
ENGINE_NAME="KeymanEngine4Mac"
TESTAPP_NAME="Keyman4Mac"
IM_NAME="Keyman4MacIM"
XCODE_PROJ_EXT=".xcodeproj"

KME4M_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$ENGINE_NAME"
KMTESTAPP_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$TESTAPP_NAME"
KM4MIM_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$IM_NAME"

KME4M_PROJECT_PATH="$KME4M_BASE_PATH/$ENGINE_NAME$XCODE_PROJ_EXT"
KMTESTAPP_PROJECT_PATH="$KMTESTAPP_BASE_PATH/$TESTAPP_NAME$XCODE_PROJ_EXT"
KMIM_WORKSPACE_PATH="$KM4MIM_BASE_PATH/$IM_NAME.xcworkspace"

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
            if [[ "$2" =~ ^(l(ocal)?)$ ]]; then
                LOCALDEPLOY=true
                CODESIGNING_SUPPRESSION=""
            elif [[ "$2" =~ ^(p(rep(release)?)?)$ ]]; then
                PREPRELEASE=true
                CONFIG="Release"
                CODESIGNING_SUPPRESSION=""
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
            if $LOCALDEPLOY || $PREPRELEASE ; then
                warn "Code-signing is required for selected deployment option."
            else
                CODESIGNING_SUPPRESSION="CODE_SIGN_IDENTITY=\"\" CODE_SIGNING_REQUIRED=NO"
            fi
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

if $DO_KEYMANENGINE ; then
    updatePlist "$KME4M_BASE_PATH" "$ENGINE_NAME"
    execBuildCommand $ENGINE_NAME "xcodebuild -project \"$KME4M_PROJECT_PATH\" $BUILD_OPTIONS $BUILD_ACTIONS $TEST_ACTION -scheme $ENGINE_NAME"
    execBuildCommand "$ENGINE_NAME dSYM file" "dsymutil \"$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework/Versions/A/$ENGINE_NAME\" -o \"$KME4M_BASE_PATH/build/$CONFIG/$ENGINE_NAME.framework.dSYM\""
fi

if $DO_KEYMANIM ; then
	cd "$KM4MIM_BASE_PATH"
	pod install
	cd "$KEYMAN_MAC_BASE_PATH"
    updatePlist "$KM4MIM_BASE_PATH" "$IM_NAME"
    execBuildCommand $IM_NAME "xcodebuild -workspace \"$KMIM_WORKSPACE_PATH\" $CODESIGNING_SUPPRESSION $BUILD_OPTIONS $BUILD_ACTIONS -scheme Keyman SYMROOT=\"$KM4MIM_BASE_PATH/build\""
    if [ "$TEST_ACTION" == "test" ]; then
    	if [ "$CONFIG" == "Debug" ]; then
    		execBuildCommand "$IM_NAME tests" "xcodebuild $TEST_ACTION -workspace \"$KMIM_WORKSPACE_PATH\" $CODESIGNING_SUPPRESSION $BUILD_OPTIONS -scheme Keyman SYMROOT=\"$KM4MIM_BASE_PATH/build\""
    	fi
    fi
fi

if $DO_KEYMANTESTAPP ; then
    updatePlist "$KMTESTAPP_BASE_PATH" "$TESTAPP_NAME"
    execBuildCommand $TESTAPP_NAME "xcodebuild -project \"$KMTESTAPP_PROJECT_PATH\" $BUILD_OPTIONS $BUILD_ACTIONS"
fi

# Deploy as requested
if $LOCALDEPLOY ; then
    displayInfo "" "Attempting local deployment with command:"
    KM4MIM_APP_BASE_PATH="$KM4MIM_BASE_PATH/build/$CONFIG"
    displayInfo "$KM4MIM_BASE_PATH/localdeploy.sh \"$KM4MIM_APP_BASE_PATH\"" 
    eval "$KM4MIM_BASE_PATH/localdeploy.sh" "$KM4MIM_APP_BASE_PATH"
    if [ $? == 0 ]; then
        displayInfo "Local deployment succeeded!" ""
    else
        fail "Local deployment failed!"
    fi
elif $PREPRELEASE ; then
    displayInfo "" "Preparing files for release deployment..."
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

displayInfo "" "Build Succeeded!"
exit 0
