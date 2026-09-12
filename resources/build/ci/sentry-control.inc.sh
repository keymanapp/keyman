# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

#
# This script centralises sentry symbol uploads and releases
# Note: We must install sentry-cli on the build agents
#


## TODO: centralise debug information file (dif) uploads


function isSentryConfigured() {
  if [ -z "${SENTRY_AUTH_TOKEN-}" ] || [ -z "${SENTRY_ORG-}" ] || [ -z "${SENTRY_URL-}" ]; then
    builder_warn "WARNING: Sentry environment variables SENTRY_AUTH_TOKEN, SENTRY_ORG and SENTRY_URL must be configured."
    return 1
  fi
  return 0
}

function isSentryCliAvailable() {
  which sentry-cli > /dev/null && return 0
  builder_warn "WARNING: sentry-cli could not be found. Skipping all sentry integration."
  return 1
}

function makeSentryRelease() {
  if isSentryConfigured; then
    if isSentryCliAvailable; then
      # This version tag matches the repository version tag release@x.y.z
      builder_echo "Making a Sentry release for tag $KEYMAN_VERSION_GIT_TAG"
      sentry-cli releases new -p keyman-android -p keyman-developer -p keyman-ios -p keyman-linux -p keyman-mac -p keyman-web -p keyman-windows $KEYMAN_VERSION_GIT_TAG

      builder_echo "Setting commits for release tag $KEYMAN_VERSION_GIT_TAG"
      sentry-cli releases set-commits --auto $KEYMAN_VERSION_GIT_TAG

      builder_echo "Finalizing release tag $KEYMAN_VERSION_GIT_TAG"
      sentry-cli releases finalize "$KEYMAN_VERSION_GIT_TAG"
    fi
  fi
}

#
# Upload sourcemaps to Sentry
#
# Parameters:
#   1: Directory containing sourcemaps to upload
#
function sentry_upload_web () {
  local SOURCEMAP_DIR="$1"
  if ! isSentryConfigured; then
    builder_warn "Skipping Sentry upload: SENTRY_ORG and/or SENTRY_PROJECT are unset."
    return
  fi

  builder_echo "Uploading ${SOURCEMAP_DIR} to Sentry..."

  sentry-cli releases files "${KEYMAN_VERSION_GIT_TAG}" upload-sourcemaps --strip-common-prefix \
    "${SOURCEMAP_DIR}" --rewrite --ext js --ext map --ext ts

  builder_echo "Upload successful."
}
