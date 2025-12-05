#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of Keyman for Android.

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/build/zip.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/android/android-actions.inc.sh"

################################ Main script ################################

# ENHANCE: In the future if we ever have OEM builds other than FV, we could go
# with `--oem=OEM` instead of `--fv` where $OEM could be a comma separated
# list, e.g. `firstvoices`.
# See https://github.com/keymanapp/keyman/pull/14222/files#r2181208750.

builder_describe \
  "Build release of Keyman for Android" \
  "all            run all actions" \
  "build          configure and build Keyman for Android" \
  "publish        publish release" \
  "--fv                               additionally build FV Android" \
  "--rsync-path=RSYNC_PATH            rsync path on remote server" \
  "--rsync-user=RSYNC_USER            rsync user on remote server" \
  "--rsync-host=RSYNC_HOST            rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT            rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM  path to help.keyman.com repository"

builder_parse "$@"

cd "${KEYMAN_ROOT}/android"

function _create_zip_archive() {
  builder_echo start "archive" "Copying release artifacts to upload/ and creating zip"

  builder_launch /android/build.sh archive

  builder_echo end "archive" success "Finished copying release artifacts to upload/ and creating zip"
}

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"

  (
    cd "${KEYMAN_ROOT}/android/upload"
    # shellcheck disable=SC2154
    tc_rsync_upload "${KEYMAN_VERSION}" "android/${KEYMAN_TIER}"
  )

  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function _publish_to_playstore() {
  local PUBTARGETS="$1"
  builder_echo start "publish to Google Play Store" "Publishing release to Google Play Store"

  builder_launch /android/build.sh "publish-play-store:${PUBTARGETS}"

  builder_echo end "publish to Google Play Store" success "Finished publishing release to Google Play Store"
}

function do_publish() {
  local PUBTARGETS
  if ! builder_is_windows; then
    # currently only tested on Windows, TODO: test cross-platform
    builder_echo error "This script is intended to be run on Windows only."
    return 1
  fi

  export RSYNC_PATH
  export RSYNC_USER
  export RSYNC_HOST
  export RSYNC_ROOT

  if builder_has_option --fv; then
    PUBTARGETS="app,fv"
  else
    # shellcheck disable=SC2034
    PUBTARGETS="app"
  fi

  _create_zip_archive
  _publish_to_downloads_keyman_com
  android_publish_symbols "${PUBTARGETS}"
  _publish_to_playstore "${PUBTARGETS}"
  tc_upload_help "Keyman for Android" android
}

if builder_has_option --fv; then
  TARGETS="engine,app,fv"
else
  # shellcheck disable=SC2034
  TARGETS="engine,app"
fi

if builder_has_action all; then
  android_clean_action
  android_build_action "${TARGETS}"
  do_publish
else
  builder_run_action  clean    android_clean_action
  builder_run_action  build    android_build_action "${TARGETS}"
  builder_run_action  publish  do_publish
fi
