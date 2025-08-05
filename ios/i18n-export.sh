#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/mac/mac.inc.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

verify_on_mac

#xcodebuild -exportLocalizations -project "engine/KMEI/KeymanEngine.xcodeproj"
#xcodebuild -exportLocalizations -project "keyman/Keyman/Keyman.xcodeproj"

BASE_EXPORT_FOLDER="build/crowdin"
START_DIRECTORY=`pwd`

readonly BASE_EXPORT_FOLDER
readonly START_DIRECTORY
readonly ARCHIVE_NAME

function exportFiles() {
  PROJECT_PATH=$1
  PROJECT=$2

  if ! [ -d "$BASE_EXPORT_FOLDER" ]; then
      mkdir -p "$BASE_EXPORT_FOLDER"
  fi

  xcodebuild -exportLocalizations -project "$PROJECT_PATH" -localizationPath "$BASE_EXPORT_FOLDER/$PROJECT"
}

# Makes this function accessible to commands
export -f exportFiles
export BASE_EXPORT_FOLDER

rm -r $BASE_EXPORT_FOLDER > /dev/null 2> /dev/null

exportFiles "engine/KMEI/KeymanEngine.xcodeproj" "engine"
exportFiles "keyman/Keyman/Keyman.xcodeproj"     "app"

echo ""
echo "---------------------------------------------"
echo "iOS base i18n files have been exported."
echo "Examine the contents of the folder at"
echo "${COLOR_GREEN}$KEYMAN_ROOT/ios/$BASE_EXPORT_FOLDER${COLOR_RESET}"
echo "and compare against the iOS entries within"
echo "${COLOR_GREEN}$KEYMAN_ROOT/crowdin.yml${COLOR_RESET}"
echo "---------------------------------------------"

