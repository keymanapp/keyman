#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" != "darwin"* ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

display_usage() {
    echo "Used to create a metadata file needed on the download site"
    echo "to describe the downloadable Keyman Input Method app."
    echo "Typically called from the Keyman mac build script."
    echo
    echo "usage: write-download_info.sh [-destDir DIR]"
    echo
    echo "  Optional switches:"
    echo "  -destDir DIR    Directory where the download_info file should be created"
    echo "                  (will be created if it does not exist)"
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

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

DEST_DIR="$KEYMAN_MACIM_BASE_PATH/output/upload"
ADD_VERSION_TO_DEST_DIR=true

KM_BLD_COUNTER="$((${VERSION##*.}))"

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -destDir)
            if [[ "$2" == "" || "$2" =~ ^\- ]]; then
                builder_die "Missing destination directory on command line."
            else
                DEST_DIR="$2"
                ADD_VERSION_TO_DEST_DIR=false
                shift # past argument
            fi
            ;;
        -h|-\?|-help|--help)
            display_usage
            ;;
        -*)
          builder_die "Unknown option $1. Run with --help for help."
          ;;
    esac
    shift # past argument
done

if $ADD_VERSION_TO_DEST_DIR ; then
    DEST_DIR="$DEST_DIR/$VERSION"
fi
if [[ ! -e $DEST_DIR ]]; then
	mkdir -p $DEST_DIR
elif [[ ! -d $DEST_DIR ]]; then
	builder_die "Destination dir exists but is not a directory: $2"
fi

DMG_FILENAME="keyman-$VERSION.dmg"
DMG_FILEPATH="$DEST_DIR/$DMG_FILENAME"
DOWNLOAD_INFO_FILEPATH="${DMG_FILEPATH}.download_info"
if [[ ! -f "$DMG_FILEPATH" ]]; then
  builder_die "Cannot compute file size or MD5 for non-existent DMG file: $DMG_FILEPATH"
fi
DMG_FILE_SIZE=$(/usr/bin/stat -f"%z" "$DMG_FILEPATH")
DMG_MD5=$(md5 -q "$DMG_FILEPATH")

if [[ -f "$DOWNLOAD_INFO_FILEPATH" ]]; then
  builder_warn "Overwriting $DOWNLOAD_INFO_FILEPATH"
fi

echo { > "$DOWNLOAD_INFO_FILEPATH"
echo "  \"name\": \"Keyman4MacIM\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"version\": \"${VERSION}\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"date\": \"$(date "+%Y-%m-%d")\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"platform\": \"mac\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"stability\": \"${TIER}\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"file\": \"${DMG_FILENAME}\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"md5\": \"${DMG_MD5}\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"type\": \"dmg\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"build\": \"${KM_BLD_COUNTER}\"," >> "$DOWNLOAD_INFO_FILEPATH"
echo "  \"size\": \"${DMG_FILE_SIZE}\"" >> "$DOWNLOAD_INFO_FILEPATH"
echo } >> "$DOWNLOAD_INFO_FILEPATH"

exit 0