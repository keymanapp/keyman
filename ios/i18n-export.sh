#!/bin/bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

KEYMAN_MAC_BASE_PATH="$KEYMAN_ROOT/mac"

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

## END STANDARD BUILD SCRIPT INCLUDE

BASE_EXPORT_FOLDER="build/localizations"
START_DIRECTORY=`pwd`
ARCHIVE_NAME="iOS_localization.zip"

readonly BASE_EXPORT_FOLDER
readonly START_DIRECTORY
readonly ARCHIVE_NAME

function exportFile() {
  # appending "ios" to the base name to match the root in crowdin
  BASE_EXPORT_FOLDER="build/localizations/ios"

  if ! [ -d "$BASE_EXPORT_FOLDER" ]; then
      mkdir -p "$BASE_EXPORT_FOLDER"
  fi

  LPROJ_SITE=$0
  LPROJ_BASE=`dirname $0`
  EXPORT_NAME="en.lproj"
  EXPORT_SITE="$BASE_EXPORT_FOLDER/$LPROJ_BASE"

  echo "Exporting $LPROJ_SITE"

  mkdir -p "$EXPORT_SITE"
  cp -r "$LPROJ_SITE" "$EXPORT_SITE"
}

# Makes this function accessible to commands
export -f exportFile
export BASE_EXPORT_FOLDER

rm -r $BASE_EXPORT_FOLDER > /dev/null 2> /dev/null

echo "" 
find . -type d -name \en.lproj ! -path "./build/*" -exec bash -c 'exportFile "$0"' {} \;

# Now to construct a clean .zip for upload to CrowdIn

cd $BASE_EXPORT_FOLDER

rm $ARCHIVE_NAME > /dev/null 2> /dev/null
if [[ -n "$WINDIR" ]]; then
  7z a "$ARCHIVE_NAME" -r ./
else
  zip -q -r $ARCHIVE_NAME . -x "**/.*" -x "__MACOSX"
fi
mv "$ARCHIVE_NAME" "../$ARCHIVE_NAME"

cd ..

ARCHIVE_DIRECTORY=`pwd`

echo ""
echo "---------------------------------------------"
echo "iOS Localization .zip is now ready for upload"
echo "See ${SUCCESS_GREEN}$ARCHIVE_DIRECTORY/$ARCHIVE_NAME${NORMAL}"
echo "---------------------------------------------"

cd $START_DIRECTORY

