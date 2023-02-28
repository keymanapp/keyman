#!/usr/bin/env bash
# Download default keyboard and lexical model packages from downloads.keyman.com

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

# Assumption: parent script that sources this already has START STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

function downloadKeyboardPackage() {
  # Check that $KEYBOARDS_TARGET is valid
  if [ "$#" -ne 2 ]; then
      builder_die "downloadKeyboardPackage requires KEYBOARD_PACKAGE_ID and KEYBOARDS_TARGET to be set"
  fi
  # Default Keyboard
  local ID="$1"
  local KEYBOARDS_TARGET="$2"

  local URL_DOWNLOAD=https://downloads.keyman.com
  local URL_API_KEYBOARD_VERSION=${URL_DOWNLOAD}/api/keyboard/

  echo "Downloading ${ID}.kmp from downloads.keyman.com"
  local URL_DOWNLOAD_FILE=`curl -s "$URL_API_KEYBOARD_VERSION/${ID}" | "$JQ" -r .kmp`
  
  # Test curl limitation, split KEYBOARDS_TARGET to DIR and FILENAME  
  local KEYBOARDS_TARGET_DIR=$(dirname $KEYBOARDS_TARGET)
  local KEYBOARDS_TARGET_FILE=$(basename $KEYBOARDS_TARGET)

  cd "$KEYBOARDS_TARGET_DIR"
  curl -f "$URL_DOWNLOAD_FILE" -o "$KEYBOARDS_TARGET_FILE" || {
      builder_die "Downloading $KEYBOARDS_TARGET failed with error $?"
  }
}

function downloadModelPackage() {
  # Check that $MODELS_TARGET is valid
  if [ "$#" -ne 2 ]; then
    builder_die "downloadModelPackage requires MODEL_PACKAGE_ID and MODELS_TARGET to be set"
  fi
  # Default Model
  local ID="$1"
  local MODELS_TARGET="$2"

  local URL_DOWNLOAD=https://downloads.keyman.com
  local URL_API_MODEL_VERSION=${URL_DOWNLOAD}/api/model/

  echo "Downloading ${ID}.model.kmp from downloads.keyman.com"
  local URL_DOWNLOAD_FILE=`curl -s "$URL_API_MODEL_VERSION/${ID}" | "$JQ" -r .kmp`

  # Test curl limitation, split MODELS_TARGET to DIR and FILENAME  
  local MODELS_TARGET_DIR=$(dirname $MODELS_TARGET)
  local MODELS_TARGET_FILE=$(basename $MODELS_TARGET)

  cd "$MODELS_TARGET_DIR"

  curl -f -s "$URL_DOWNLOAD_FILE" -o "$MODELS_TARGET_FILE" || {
      builder_die "Downloading $MODELS_TARGET failed with error $?"
  }
}
