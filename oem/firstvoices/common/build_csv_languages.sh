#!/usr/bin/env bash

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

display_usage ( ) {
  echo "build_csv_languages.sh  [-h|-?]"
  echo "Extract kmp.json from fv_all.kmp and parse it to populate keyboards.csv "
  echo "with keyboard version and associated language info"
  echo ""
  exit 1
}

export TARGET=FirstVoices
KEYBOARD_PACKAGE_ID="fv_all"
KEYBOARDS_TARGET="$KEYMAN_ROOT/oem/firstvoices/${KEYBOARD_PACKAGE_ID}.kmp"
KEYBOARDS_CSV="$KEYMAN_ROOT/oem/firstvoices/keyboards.csv"
KEYBOARDS_CSV_TEMP="${KEYBOARDS_CSV}.tmp"

## Parses kmp.json from fv_all.kmp and add the keyboard version and the
## first associated language ID/name for each keyboard in the existing keyboards.csv
## Writes out to the target keyboards.csv
function updateLanguages {
  # Verify keyboards.csv exists
  if [[ ! -f "$KEYBOARDS_CSV" ]]; then
    echo "$KEYBOARDS_CSV required."
    exit 1
  fi

  # Download fv_all.kmp and extract kmp.json to get keyboard versions and language info
  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "$KEYBOARDS_TARGET"
  unzip -o "$KEYBOARDS_TARGET" kmp.json

  local KMP_FILE="kmp.json"
  if [ ! -f "$KMP_FILE" ]; then
    echo "ERROR: kmp.json not extracted"
    exit 1
  fi

  if [ -f $KEYBOARDS_CSV_TEMP ]; then
    echo "Cleaning existing keyboards.csv.tmp"
    rm -f "${KEYBOARDS_CSV_TEMP}"
  fi

  echo "Parsing language info to add to ${KEYBOARDS_CSV_TEMP}"

  # Existing column headers
  while IFS="," read -r SHORTNAME ID NAME REGION WEB_KEYBOARD_9_0
  do
    if [ "$SHORTNAME" != "Shortname" ]; then
      # Find first matching language info for each keyboard
      local KEYBOARD_INFO=$(cat "${KMP_FILE}" | "$JQ" -r ".keyboards[] | select(.id==\"$ID\")")
      local KEYBOARD_VERSION=$(echo ${KEYBOARD_INFO} | "$JQ" -r ".version")
      local LANGUAGE_INFO=$(echo ${KEYBOARD_INFO} | "$JQ" -r ".languages[0]")
      local LANGUAGE_ID=$(echo ${LANGUAGE_INFO} | "$JQ" -r ".id")
      local LANGUAGE_NAME=$(echo ${LANGUAGE_INFO} | "$JQ" -r ".name")

      # trim newline from WEB_KEYBOARD_9_0
      WEB_KEYBOARD_9_0=${WEB_KEYBOARD_9_0%$'\n'}
      WEB_KEYBOARD_9_0=${WEB_KEYBOARD_9_0%$'\r'}

      # Write out keyboard info
      echo "${SHORTNAME},${ID},${NAME},${REGION},${WEB_KEYBOARD_9_0},${KEYBOARD_VERSION},${LANGUAGE_ID},${LANGUAGE_NAME}" >> $KEYBOARDS_CSV_TEMP
    else
      # Copy header line to target
      echo "Shortname,ID,Name,Region,9.0 Web Keyboard,Version,Language ID,Language Name" >> $KEYBOARDS_CSV_TEMP
    fi
  done < $KEYBOARDS_CSV

  # Overwrite keyboards.csv with updated temp file and cleanup
  mv -f "$KEYBOARDS_CSV_TEMP" "$KEYBOARDS_CSV"
  rm -f "$KEYBOARDS_TARGET"
  rm -f "$KMP_FILE"

  echo "Language info added to ${KEYBOARDS_CSV}"
}

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -h|-\?)
      display_usage
      ;;
  esac
  shift
done

updateLanguages
