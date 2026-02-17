# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/jq.inc.sh"

_utils_inc_sh=1

#
# Write ${UPLOAD_DIR}/${ARTIFACT_FILENAME}.download_info file for the target
# artifact
#
# Parameters:
#   1: UPLOAD_DIR          Directory where artifact can be found
#   2: ARTIFACT_FILENAME   Filename (without path) of artifact
#   3: ARTIFACT_NAME       Descriptive name of artifact
#   4: ARTIFACT_TYPE       File extension of artifact, without initial period (e.g. tar.xz)
#   5: PLATFORM            Target platform for artifact
#
# TODO: Move to CI include?
write_download_info() {
  local UPLOAD_DIR="$1"
  local ARTIFACT_FILENAME="$2"
  local ARTFIACT_NAME="$3"
  local ARTIFACT_TYPE="$4"
  local PLATFORM="$5"

  local DATE HASH SIZE DOWNLOAD_INFO STAT_FLAGS

  if [[ ! "${PLATFORM}" =~ ^(android|ios|linux|mac|web|win)$ ]]; then
    builder_die "Invalid project ${PLATFORM}"
  fi

  # shellcheck disable=SC2312
  if builder_is_macos && [[ $(command -v stat) == /usr/bin/stat ]]; then
    # /usr/bin/stat on mac is BSD
    STAT_FLAGS="-f%z"
  else
    # GNU (coreutils)
    STAT_FLAGS="--print=%s"
  fi

  # Construct .download_info
  DATE=$(date +%F)
  # shellcheck disable=SC2312
  HASH=$(md5sum "${UPLOAD_DIR}/${ARTIFACT_FILENAME}" | cut -d ' ' -f 1)
  # shellcheck disable=SC2248
  SIZE=$(stat ${STAT_FLAGS} "${UPLOAD_DIR}/${ARTIFACT_FILENAME}")

  # shellcheck disable=SC2016,SC2154
  DOWNLOAD_INFO=$(
    "${JQ}" -n \
    --arg NAME "${ARTFIACT_NAME}" \
    --arg BUILD_NUMBER "${KEYMAN_VERSION}" \
    --arg DATE "${DATE}" \
    --arg PLATFORM "${PLATFORM}" \
    --arg KEYMAN_TIER "${KEYMAN_TIER}" \
    --arg FILENAME "${ARTIFACT_FILENAME}" \
    --arg ARTIFACT_TYPE "${ARTIFACT_TYPE}" \
    --arg HASH "${HASH}" \
    --arg BUILD_COUNTER "${KEYMAN_VERSION_PATCH}" \
    --arg SIZE "${SIZE}" \
    '{
      name: $NAME, version: $BUILD_NUMBER, date: $DATE, platform: $PLATFORM,
      stability: $KEYMAN_TIER, file: $FILENAME, md5: $HASH, type: $ARTIFACT_TYPE,
      build: $BUILD_COUNTER, size: $SIZE
    }'
  )
  echo "${DOWNLOAD_INFO}" | "${JQ}" . >> "${UPLOAD_DIR}/${ARTIFACT_FILENAME}.download_info"
}

#
# Re-runs the specified command-line instruction up to 5 times should it fail, waiting a
# random delay between each attempt.  No re-runs are attempted after successful commands.
#
# ### Usage
#   try_multiple_times command [param1 param2...]
#
# ### Parameters
#   1: $@         command-line arguments
try_multiple_times ( ) {
  _try_multiple_times 0 "$@"
}

# $1  The current retry count
# $2+ (everything else) the command to retry should it fail
_try_multiple_times ( ) {
  local RETRY_MAX=5

  # Configuration:  wait between 10 sec and 120 sec.

  # in seconds.
  local RETRY_MAX_WAIT_RANGE=111
  # in seconds
  local RETRY_MIN_WAIT=10

  local retryCount=$1
  shift

  if (( "$retryCount" == "$RETRY_MAX" )); then
    builder_die "Retry limit of $RETRY_MAX attempts reached."
  fi

  retryCount=$(( $retryCount + 1 ))

  if (( $retryCount != 1 )); then
    local wait_length=$(( RANDOM % RETRY_MAX_WAIT_RANGE + RETRY_MIN_WAIT ))
    builder_echo "Delaying $wait_length seconds before attempt $retryCount: \`$@\`"
    sleep $wait_length
  fi

  local code=0
  "$@" || code=$?
  if (( $code != 0 )); then
    builder_echo "Command failed with error $code"
    _try_multiple_times $retryCount "$@"
  fi
}

#
# Wrapper for check-markdown tool to verify links within and validity of all .md
# files in a folder and its sub-folders
#
# Usage:
#   check-markdown path_root
# Parameters:
#   1: path_root    path to base folder containing .md files to be checked
#
check-markdown() {
  node "$KEYMAN_ROOT/resources/tools/check-markdown" --root "$1"
}

