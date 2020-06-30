#!/bin/bash
# Download default keyboard and lexical model packages from downloads.keyman.com

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
#set -e
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

function downloadPackages() {
  echo "Downloading default keyboard & dictionary from downloads.keyman.com"

  # Check that $KEYBOARDS_TARGET is valid
  if [[ -z "$KEYBOARDS_TARGET" ]]; then
    die "KEYBOARDS_TARGET cannot be empty"
  fi

  # Check that $MODELS_TARGET is valid
  if [[ -z "$MODELS_TARGET" ]]; then
    die "MODELS_TARGET cannot be empty"
  fi

  URL_DOWNLOAD=https://downloads.keyman.com
  URL_API_KEYBOARD_VERSION=${URL_DOWNLOAD}/api/keyboard/
  URL_API_MODEL_VERSION=${URL_DOWNLOAD}/api/model/

  # Default Keyboard
  id="sil_euro_latin"
  echo "Downloading $id"
  URL_DOWNLOAD_FILE=`curl -s "$URL_API_KEYBOARD_VERSION/$id" | "$JQ" -r .kmp`
  curl -f -s "$URL_DOWNLOAD_FILE" -o "$KEYBOARDS_TARGET"

  if [ $? -ne 0 ]; then
      die "Downloading $KEYBOARDS_TARGET failed"
  fi

  # Default Model
  id="nrc.en.mtnt"
  echo "Downloading $id"
  URL_DOWNLOAD_FILE=`curl -s "$URL_API_MODEL_VERSION/$id" | "$JQ" -r .kmp`
  curl -f -s "$URL_DOWNLOAD_FILE" -o "$MODELS_TARGET"

  if [ $? -ne 0 ]; then
      die "Downloading $MODELS_TARGET failed"
  fi
}
