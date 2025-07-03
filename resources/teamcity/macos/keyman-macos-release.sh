#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman macOS/Release
#
# Requires: env variable $MAC_BUILDAGENT_PASSWORD set

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-mac.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/macos/macos-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build release of Keyman for macOS" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          build Keyman for macOS" \
  "publish        publish Keyman for macOS" \
  "--rsync-path=RSYNC_PATH              rsync path on remote server" \
  "--rsync-user=RSYNC_USER              rsync user on remote server" \
  "--rsync-host=RSYNC_HOST              rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT              rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM    path to help.keyman.com repository"

builder_parse "$@"

cd "${KEYMAN_ROOT}/mac"

function _publish_to_downloads_keyman_com() {
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"

  local UPLOAD_DIR

  # Note that the `rsync` tool that exists on Macs by default is a very old version and must be updated to a newer version, e.g. by using Homebrew.

  UPLOAD_DIR="Keyman4MacIM/output/upload/${KEYMAN_VERSION}"

  # Set permissions as required on download site
  builder_echo "Setting upload file permissions for downloads.keyman.com"
  chmod g+w  "${UPLOAD_DIR}"
  chmod a+rx "${UPLOAD_DIR}"

  chmod g+rw "${UPLOAD_DIR}"/*
  chmod a+r  "${UPLOAD_DIR}"/*

  builder_echo "Performing rsync call"
  # shellcheck disable=SC2154
  tc_rsync_upload "${UPLOAD_DIR}" "mac/${KEYMAN_TIER}"

  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function _build_publish() {
  builder_echo start "publish" "Publishing Keyman for macOS"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/mac/build.sh" publish
  builder_echo end success "publish" "Finished publishing Keyman for macOS"
}

function _do_publish() {
  _build_publish
  _publish_to_downloads_keyman_com
  tc_upload_help "Keyman for macOS" mac
}

ba_mac_unlock_keychain

if builder_has_action all; then
  macos_clean_action
  macos_build_action
  do_publish
else
  builder_run_action  clean    macos_clean_action
  builder_run_action  build    macos_build_action
  builder_run_action  publish  do_publish
fi
