#!/usr/bin/env bash
# Download default keyboard and lexical model packages from downloads.keyman.com
# Retries up to 5 times

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
  local RETRY=5       # Curl retries this number of times before giving up
  local RETRY_DELAY=5 # Make curl sleep this amount of time before each retry when a transfer has failed

  echo "Downloading ${ID}.kmp from downloads.keyman.com up to ${RETRY} attempts"
  local URL_DOWNLOAD_FILE=`curl --silent "$URL_API_KEYBOARD_VERSION/${ID}" | "$JQ" -r .kmp`
  curl --fail --retry "$RETRY" --retry-delay "$RETRY_DELAY" --silent "$URL_DOWNLOAD_FILE" --output "$KEYBOARDS_TARGET" || {
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
  local RETRY=5       # Curl retries this number of times before giving up
  local RETRY_DELAY=5 # Make curl sleep this amount of time before each retry when a transfer has failed

  echo "Downloading ${ID}.model.kmp from downloads.keyman.com up to ${RETRY} attempts"
  local URL_DOWNLOAD_FILE=`curl --silent "$URL_API_MODEL_VERSION/${ID}" | "$JQ" -r .kmp`
  curl --fail --retry "$RETRY" --retry-delay "$RETRY_DELAY" --silent "$URL_DOWNLOAD_FILE" --output "$MODELS_TARGET" || {
      builder_die "Downloading $MODELS_TARGET failed with error $?"
  }
}
