#!/bin/sh

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" != "darwin"* ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

display_usage() {
    echo "Used to create a disk image containing the Keyman Input Method app."
    echo "Typically called from the Keyman mac build script."
    echo
    echo "usage: make-km-dmg.sh -version #.#.#"
    echo
    echo "  -version #.#.#  Specifies the build version number, which should be in the"
    echo "                  form Major.Minor.BuildCounter"
    echo "Optional switches:"
    echo "  -sourceApp APP  The Keyman.app to put into the DMG"
    echo "  -template DMG   The DMG image to use as a template (with background image, etc.)"
    echo "  -destDir DIR    Directory where the dmg file should be created (will be"
    echo "                  created if it does not exist)"
    echo "  -quiet          Do not print any output except for warnings and errors. If an"
    echo "                  hdiutil operation fails, no details of the failure will be given."
    echo "  -verbose        Be verbose: produce extra progress output and error diagnostics."
    echo "                  This option can help decipher why a particular hdiutil operation"
    echo "                  failed. At a minimum, the probing of any specified images will"
    echo "                  be detailed."
    echo "  -debug          Output a large amount of progress information."
    echo ""
    echo "Note that this script creates/modifies contents of output folder in the directory"
    echo "where this script resides."

    exit 1
}

KEYMAN_MACIM_BASE_PATH="${BASH_SOURCE[0]}";
if ([ -h "${KEYMAN_MACIM_BASE_PATH}" ]) then
	while([ -h "${KEYMAN_MACIM_BASE_PATH}" ]) do KEYMAN_MACIM_BASE_PATH=`readlink "${KEYMAN_MACIM_BASE_PATH}"`; done
fi
pushd . > /dev/null
cd `dirname ${KEYMAN_MACIM_BASE_PATH}` > /dev/null
KEYMAN_MACIM_BASE_PATH=`pwd`;
popd  > /dev/null

. $KEYMAN_MACIM_BASE_PATH/../bashHelperFunctions.sh

KM_APP_NAME="Keyman.app"
SOURCE_KM_APP="$KEYMAN_MACIM_BASE_PATH/build/Release/$KM_APP_NAME"
OUTPUT_DIR="$KEYMAN_MACIM_BASE_PATH/output"
STAGING_DIR="$OUTPUT_DIR/temp"
DEST_DIR="$OUTPUT_DIR/upload"
ADD_VERSION_TO_DEST_DIR=true
TEMPLATE_IMAGE="$KEYMAN_MACIM_BASE_PATH/Keyman-template.dmg"
VERBOSITY=""
QUIET=false

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -version)
            assertValidVersionNbr "$2"
            KM_VERSION="$2"
            KM_BLD_COUNTER="$((${KM_VERSION##*.}))"
            shift # past argument
            ;;
        -sourceApp)
            if [[ "$2" == "" || "$2" =~ ^\- ]]; then
                fail "Missing source directory on command line."
            else
                SOURCE_KM_APP=="$2"
                shift # past argument
            fi
            ;;
        -template)
            if [[ "$2" == "" || "$2" =~ ^\- ]]; then
                fail "Missing template image on command line."
            elif ! [[ "$2" =~ \.dmg$ ]]; then
                fail "Template image must be a .dmg!"
            else
                TEMPLATE_IMAGE="$2"
                shift # past argument
            fi
            ;;
        -destDir)
            if [[ "$2" == "" || "$2" =~ ^\- ]]; then
                fail "Missing destination directory on command line."
            else
                DEST_DIR="$2"
                ADD_VERSION_TO_DEST_DIR=false
                shift # past argument
            fi
            ;;
        -quiet)
            VERBOSITY="-quiet"
            QUIET=true
            ;;
        -verbose)
            VERBOSITY="-verbose"
            ;;
        -debug)
            VERBOSITY="-debug"
            ;;
        -h|-?|-help|--help)
            display_usage
            ;;
        -*)
          fail "Unknown option $1. Run with --help for help."
          ;;
    esac
    shift # past argument
done

# Step 0 - check parameter and initial file state
if [ "$KM_VERSION" = "" ]; then
  fail "Required -version parameter not specified!"
fi

if [[ ! -d "$SOURCE_KM_APP" ]]; then
	fail "$SOURCE_KM_APP does not exist!"
fi

if [[ ! -f "$TEMPLATE_IMAGE" ]]; then
	fail "$TEMPLATE_IMAGE does not exist!"
fi

if [[ ! -e "$OUTPUT_DIR" ]]; then
	mkdir "$OUTPUT_DIR"
elif [[ ! -d "$OUTPUT_DIR" ]]; then
	fail "Output dir exists but is not a directory: $2"
fi

if $ADD_VERSION_TO_DEST_DIR ; then
    DEST_DIR="$DEST_DIR/$KM_VERSION"
fi
if [[ ! -e "$DEST_DIR" ]]; then
	mkdir -p "$DEST_DIR"
elif [[ ! -d "$DEST_DIR" ]]; then
	fail "Destination dir exists but is not a directory: $2"
fi

# Step 1 - Copy template to working copy to prevent unintended changes
WORKING_COPY_OF_IMAGE="$OUTPUT_DIR/Keyman-temp-$KM_VERSION.dmg"
displayInfo "Copying \"$TEMPLATE_IMAGE\" to \"$WORKING_COPY_OF_IMAGE\"..."
if [[ -e "$WORKING_COPY_OF_IMAGE" && "$VERBOSITY" != "-quiet" ]] ; then
    warn "Overwriting: $WORKING_COPY_OF_IMAGE"
fi
cp -f "$TEMPLATE_IMAGE" "$WORKING_COPY_OF_IMAGE"

# Step 2 - Mount (copy of) template image - writeable
displayInfo "Attaching \"$WORKING_COPY_OF_IMAGE\"" "Mounting as \"$STAGING_DIR\"..."
if [[ -e "$STAGING_DIR" && "$VERBOSITY" != "-quiet" ]] ; then
    warn "Overwriting: $STAGING_DIR"
    rm -rf "$STAGING_DIR"
fi
hdiutil attach "$WORKING_COPY_OF_IMAGE" -readwrite -mountpoint "$STAGING_DIR" $VERBOSITY
if  ! [[ $? == 0 && -d "$STAGING_DIR" ]] ; then
    fail "Image could not be mounted at: \"$STAGING_DIR\""
fi
DEST_KM_APP="$STAGING_DIR/$KM_APP_NAME"
if  ! [[ -d "$DEST_KM_APP" ]] ; then
    hdiutil detach $STAGING_DIR $VERBOSITY
    fail "Expected mounted image to contain Keyman app, but \"$DEST_KM_APP\" does not exist."
fi

# Step 3 - Replace existing application files with new version
displayInfo "Copying files from \"$SOURCE_KM_APP\"..."
find "$DEST_KM_APP" -mindepth 1 -maxdepth 1 -print0 | xargs -0 rm -rf
cp -fR "$SOURCE_KM_APP/" "$DEST_KM_APP"

# Step 4 - Detach/unmount the temporary image/staging area
displayInfo "Detaching \"$WORKING_COPY_OF_IMAGE\""
hdiutil detach $STAGING_DIR $VERBOSITY
if  [[ $? != 0 || -d "$STAGING_DIR" ]] ; then
    fail "Failed to unmount: \"$STAGING_DIR\""
fi

# Step 5 - Convert image to a compressed readonly DMG image
DMG_FILE_PATH="$DEST_DIR/Keyman-$KM_VERSION.dmg"
displayInfo "Converting/compressing image to create \"$DMG_FILE_PATH\""
if [[ -e "$DMG_FILE_PATH" ]] ; then
    if [[ "$VERBOSITY" != "-quiet" ]] ; then
        warn "Overwriting: $DMG_FILE_PATH"
    fi
    rm -rf "$DMG_FILE_PATH"
fi
hdiutil convert "$WORKING_COPY_OF_IMAGE" -format UDZO -o "$DMG_FILE_PATH" $VERBOSITY
if ! [[ $? == 0 && -f "$DMG_FILE_PATH" ]]; then
    fail "make-km-dmg failed to create \"$DMG_FILE_PATH\"."
fi

# Step 6 - Clean up
displayInfo "Cleaning up..."
rm "$WORKING_COPY_OF_IMAGE"

displayInfo "Make DMG completed successfully."
exit 0