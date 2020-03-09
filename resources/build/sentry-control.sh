#!/bin/bash

#
# This script centralises sentry symbol uploads and releases
#

#
# if [ -f ]
#
# TODO: centralise debug information file (dif) uploads

## We'll include sentry-cli on the build agents

# TODO figure out who will include build-utils.sh
#. build-utils.sh

function isSentryConfigured() {
  if [ -z ${SENTRY_AUTH_TOKEN-} ] || [ -z ${SENTRY_ORG} ] || [ -z ${SENTRY_URL} ]; then
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
      local SENTRY_RELEASE_VERSION="release-$VERSION_WITH_TAG"
      echo "Making a Sentry release for tag $SENTRY_RELEASE_VERSION"
      # sentry-cli releases new -p keyman-android -p keyman-developer -p keyman-ios -p keyman-linux -p keyman-mac -p keyman-web -p keyman-windows $SENTRY_RELEASE_VERSION
      sentry-cli releases new -p keyman-windows $SENTRY_RELEASE_VERSION

      echo "Setting commits for release tag $SENTRY_RELEASE_VERSION"
      sentry-cli releases set-commits --auto $SENTRY_RELEASE_VERSION
    fi
  fi
}

