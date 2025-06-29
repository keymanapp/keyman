# Keyman is copyright (C) SIL Global. MIT License.

. "${KEYMAN_ROOT}/resources/build/jq.inc.sh"

#
# Write ${UPLOAD_DIR}/${ARTIFACT_FILENAME}.download_info file for the target
# artifact
#
# Parameters:
#   1: UPLOAD_DIR          Directory where artifact can be found
#   2: ARTIFACT_FILENAME   Filename (without path) of artifact
#   3: ARTIFACT_NAME       Descriptive name of artifact
#   4: ARTIFACT_TYPE       File extension of artifact, without initial period (e.g. tar.gz)
#   5: PLATFORM            Target platform for artifact
#
write_download_info() {
  # TODO: DRY out with Linux download_info writer, others
  local UPLOAD_DIR="$1"
  local ARTIFACT_FILENAME="$2"
  local ARTFIACT_NAME="$3"
  local ARTIFACT_TYPE="$4"
  local PLATFORM="$5"

  local DATE HASH SIZE DOWNLOAD_INFO

  # Construct .download_info
  DATE=$(date +%F)
  HASH=$(md5sum "${UPLOAD_DIR}/${ARTIFACT_FILENAME}" | cut -d ' ' -f 1)
  SIZE=$(stat --print="%s" "${UPLOAD_DIR}/${ARTIFACT_FILENAME}")

  DOWNLOAD_INFO=$(
    "$JQ" -n \
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
  echo "${DOWNLOAD_INFO}" | "$JQ" . >> "${UPLOAD_DIR}/${ARTIFACT_FILENAME}.download_info"
}