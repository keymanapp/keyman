#!/bin/bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BASE_DIR="$KEYMAN_ROOT/ios"

verify_on_mac

#xcodebuild -exportLocalizations -project "engine/KMEI/KeymanEngine.xcodeproj"
#xcodebuild -exportLocalizations -project "keyman/Keyman/Keyman.xcodeproj" 

# TODO:  REWORK!
function generateStringFiles() {
  # Checks all project files (under current directory) for
  # i18n-able strings.
  find . -name \*.swift | tr '\n' '\0' | xargs -0 genstrings -o en.lproj
}

KMEA_BASE=engine/KMEI/KeymanEngine
APP_BASE=keyman/Keyman/Keyman

cd $KMEA_BASE

generateStringFiles

cd $BASE_DIR
cd $APP_BASE

generateStringFiles

cd $BASE_DIR