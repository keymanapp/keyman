#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of Keyman for Windows.

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
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-windows.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/windows/windows-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build and publish Keyman for Windows" \
  "all            run all actions" \
  "build          build Keyman for Windows Windows" \
  "test           run Keyman for Windows tests" \
  "publish        publish release" \
  "--rsync-path=RSYNC_PATH            rsync path on remote server" \
  "--rsync-user=RSYNC_USER            rsync user on remote server" \
  "--rsync-host=RSYNC_HOST            rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT            rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM  path to help.keyman.com repository"

builder_parse "$@"

cd "${KEYMAN_ROOT}/windows/src"

if ! is_windows; then
  builder_echo error "This script is intended to be run on Windows only."
  exit 1
fi

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"

  (
    cd "${KEYMAN_ROOT}/windows"
    # shellcheck disable=SC2154
    powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/publish-windows-to-downloads-keyman-com.ps1"
  )

  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function windows_publish_action() {
  builder_echo start "publish windows" "Publishing Keyman for Windows"

  export RSYNC_PATH
  export RSYNC_USER
  export RSYNC_HOST
  export RSYNC_ROOT

  "${KEYMAN_ROOT}/windows/build.sh" publish
  windows_upload_symbols_to_sentry
  download_symbol_server_index
  publish_new_symbols
  _publish_to_downloads_keyman_com
  upload_help "Keyman for Windows" windows
  builder_echo end "publish windows" success "Finished publishing Keyman for Windows"
}

if builder_has_action all; then
  windows_build_action
  windows_test_action
  windows_publish_action
else
  builder_run_action  build       windows_build_action
  builder_run_action  test        windows_test_action
  builder_run_action  publish     windows_publish_action
fi

