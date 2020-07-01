#!/bin/bash
# Download default keyboard and lexical model packages from downloads.keyman.com

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/./build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

function downloadKeyboardPackage() {
  # Check that $KEYBOARDS_TARGET is valid
  if [ "$#" -ne 1 ]; then
      die "downloadKeyboardPackage requires KEYBOARDS_TARGET to be set"
  fi
  local KEYBOARDS_TARGET="$1"

  local URL_DOWNLOAD=https://downloads.keyman.com
  local URL_API_KEYBOARD_VERSION=${URL_DOWNLOAD}/api/keyboard/

  # Default Keyboard
  local id="sil_euro_latin"
  echo "Downloading ${id}.kmp from downloads.keyman.com"
  local URL_DOWNLOAD_FILE=`curl -s "$URL_API_KEYBOARD_VERSION/${id}" | "$JQ" -r .kmp`
  curl -f -s "$URL_DOWNLOAD_FILE" -o "$KEYBOARDS_TARGET" || {
      die "Downloading $KEYBOARDS_TARGET failed with error $?"
  }
}

function downloadModelPackage() {
  # Check that $MODELS_TARGET is valid
  if [ "$#" -ne 1 ]; then
    die "downloadModelPackage requires MODELS_TARGET to be set"
  fi
  local MODELS_TARGET="$1"

  local URL_DOWNLOAD=https://downloads.keyman.com
  local URL_API_MODEL_VERSION=${URL_DOWNLOAD}/api/model/

  # Default Model
  local id="nrc.en.mtnt"
  echo "Downloading ${id}.model.kmp from downloads.keyman.com"
  local URL_DOWNLOAD_FILE=`curl -s "$URL_API_MODEL_VERSION/${id}" | "$JQ" -r .kmp`
  curl -f -s "$URL_DOWNLOAD_FILE" -o "$MODELS_TARGET" || {
      die "Downloading $MODELS_TARGET failed with error $?"
  }  
}
