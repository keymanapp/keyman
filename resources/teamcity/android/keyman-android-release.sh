#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of Keyman for Android.

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
. "${KEYMAN_ROOT}/resources/teamcity/android/android-actions.inc.sh"

################################ Main script ################################

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

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"

  # shellcheck disable=SC2154
  powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/publish-android-to-downloads-keyman-com.ps1"

  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function _publish_to_playstore() {
  builder_echo start "publish to Google Play Store" "Publishing release to Google Play Store"

  "${KEYMAN_ROOT}/android/build.sh" "publish:${PUBTARGETS}" --ci

  builder_echo end "publish to Google Play Store" success "Finished publishing release to Google Play Store"
}

function do_build() {
  android_build_action
}

function do_publish() {
  if ! is_windows; then
    # requires Powershell, so currently only supported on Windows
    builder_echo error "This script is intended to be run on Windows only."
    return 1
  fi

  export RSYNC_PATH
  export RSYNC_USER
  export RSYNC_HOST
  export RSYNC_ROOT

  _publish_to_downloads_keyman_com
  _publish_to_playstore
  upload_help "Keyman for Android" android
}

if builder_has_option --fv; then
  TARGETS="engine,app,fv"
  PUBTARGETS="app,fv"
else
  # shellcheck disable=SC2034
  TARGETS="engine,app"
  PUBTARGETS="app"
fi

if builder_has_action all; then
  android_clean_action
  do_build
  do_publish
else
  builder_run_action  clean    android_clean_action
  builder_run_action  build    do_build
  builder_run_action  publish  do_publish
fi
