#!/usr/bin/env bash

#
# This script centralises sentry symbol uploads and releases
# Note: We must install sentry-cli on the build agents
#


## TODO: centralise debug information file (dif) uploads


function isSentryConfigured() {
  if [ -z "${SENTRY_AUTH_TOKEN-}" ] || [ -z "${SENTRY_ORG-}" ] || [ -z "${SENTRY_URL-}" ]; then
    echo "WARNING: Sentry environment variables SENTRY_AUTH_TOKEN, SENTRY_ORG and SENTRY_URL must be configured."
    return 1
  fi
  return 0
}

function isSentryCliAvailable() {
  which sentry-cli > /dev/null && return 0
  echo "WARNING: sentry-cli could not be found. Skipping all sentry integration."
  return 1
}

function makeSentryRelease() {
  if isSentryConfigured; then
    if isSentryCliAvailable; then
      # This version tag matches the repository version tag release@x.y.z
      echo "Making a Sentry release for tag $VERSION_GIT_TAG"
      sentry-cli releases new -p keyman-android -p keyman-developer -p keyman-ios -p keyman-linux -p keyman-mac -p keyman-web -p keyman-windows $VERSION_GIT_TAG

      echo "Setting commits for release tag $VERSION_GIT_TAG"
      sentry-cli releases set-commits --auto $VERSION_GIT_TAG

      echo "Finalizing release tag $VERSION_GIT_TAG"
      sentry-cli releases finalize "$VERSION_GIT_TAG"
    fi
  fi
}

