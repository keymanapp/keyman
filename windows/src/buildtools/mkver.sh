#!/bin/bash

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage() {
  echo "Usage: mkver.sh <file.in> <file.out>"
  exit 1
}

if [[ $# -lt 2 ]]; then
  display_usage
fi

replaceVersionStrings_Mkver() {
  # This is similar to replaceVersionStrings in build-utils.sh but supports all
  # the old mkver strings
  local infile=$1
  local outfile=$2

  sed "
    s/\$VersionWin/$VERSION_WIN/g;
    s/\$VersionRelease/$VERSION_RELEASE/g;
    s/\$VersionMajor/$VERSION_MAJOR/g;
    s/\$VersionMinor/$VERSION_MINOR/g;
    s/\$VersionPatch/$VERSION_PATCH/g;
    s/\$Tier/$TIER/g;
    s/\$Tag/$VERSION_TAG/g;
    s/\$VersionWithTag/$VERSION_WITH_TAG/g;
    s/\$VersionRc/$VERSION_MAJOR,$VERSION_MINOR,$VERSION_PATCH,0/g;
    s/\$Environment/$VERSION_ENVIRONMENT/g;
    s/\$Version/$VERSION/g;
    s/\$VERSIONNUM/$VERSION_MAJOR,$VERSION_MINOR,$VERSION_PATCH,0/g;
    s/\$VERSION/$VERSION_WIN/g;
    s/\$RELEASE_MAJOR/$VERSION_MAJOR/g;
    s/\$RELEASE_MINOR/$VERSION_MINOR/g;
    s/\$RELEASE/$VERSION_RELEASE/g;
    " "$infile" > "$outfile"
}

replaceVersionStrings_Mkver "$1" "$2"
