#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of Keyman Developer

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-windows.inc.sh"

################################ Main script ################################

builder_describe \
  "Build release of Keyman Developer" \
  "all            run all actions" \
  "build          build Keyman Developer and test keyboards" \
  "publish        publish release of Keyman Developer" \
  "--rsync-path=RSYNC_PATH                    rsync path on remote server" \
  "--rsync-user=RSYNC_USER                    rsync user on remote server" \
  "--rsync-host=RSYNC_HOST                    rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT                    rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM          path to help.keyman.com repository" \
  "--symbols-local-path=LOCAL_SYMBOLS_PATH    local path to symbols directory" \
  "--symbols-remote-path=REMOTE_SYMBOLS_PATH  remote path to symbols directory" \
  "--symbols-subdir=SYMBOLS_SUBDIR            subdirectory containing symbols"

builder_parse "$@"

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/developer/src"

function _build_developer() {
  builder_echo start "build developer" "Building Keyman Developer"

  builder_launch /developer/src/build.sh configure build test api publish

  builder_echo end "build developer" success "Finished building Keyman Developer"
}

function _build_testkeyboards() {
  builder_echo start "build testkeyboards" "Building test keyboards"

  builder_launch /common/test/keyboards/build.sh

  builder_echo end "build testkeyboards" success "Finished building test keyboards"
}

function _publish_sentry() {
  builder_echo start "publish sentry" "Publishing debug information files to Sentry"

  builder_launch /developer/src/tools/sentry-upload-difs.sh

  builder_echo end "publish sentry" success "Finished publishing debug information files to Sentry"
}

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"

  (
    cd "${KEYMAN_ROOT}/developer"

    local UPLOAD_PATH KEYBOARDS_PATH KMCOMP_ZIP DEVELOPER_EXE DEBUG_ZIP
    # shellcheck disable=SC2154
    UPLOAD_PATH="${KEYMAN_ROOT}/developer/upload/${KEYMAN_VERSION}"
    KEYBOARDS_PATH="${UPLOAD_PATH}/keyboards"
    KMCOMP_ZIP="kmcomp-${KEYMAN_VERSION}.zip"
    DEVELOPER_EXE="keymandeveloper-${KEYMAN_VERSION}.exe"
    DEBUG_ZIP="debug-${KEYMAN_VERSION}.zip"

    rm -rf "${UPLOAD_PATH}"
    mkdir -p "${UPLOAD_PATH}"

    cp "${KEYMAN_ROOT}/developer/release/${KEYMAN_VERSION}/${DEVELOPER_EXE}" "${UPLOAD_PATH}/"
    cp "${KEYMAN_ROOT}/developer/release/${KEYMAN_VERSION}/${KMCOMP_ZIP}"    "${UPLOAD_PATH}/"

    write_download_info "${UPLOAD_PATH}" "${DEVELOPER_EXE}" "Keyman Developer" exe win
    write_download_info "${UPLOAD_PATH}" "${KMCOMP_ZIP}" "Keyman Developer Command-Line Compiler" zip win

    mkdir -p "${KEYBOARDS_PATH}"
    cp -r "${KEYMAN_ROOT}/common/test/keyboards"/*/build/*.kmp "${KEYBOARDS_PATH}/"

    if [[ -f "${KEYMAN_ROOT}/developer/release/${KEYMAN_VERSION}/${DEBUG_ZIP}" ]]; then
      cp "${KEYMAN_ROOT}/developer/release/${KEYMAN_VERSION}/${DEBUG_ZIP}" "${UPLOAD_PATH}/"
      write_download_info "${UPLOAD_PATH}" "${DEBUG_ZIP}" "Keyman Developer debug files" zip win
    fi

    cd "${KEYMAN_ROOT}/developer/upload"
    # shellcheck disable=SC2154
    tc_rsync_upload "${KEYMAN_VERSION}" "developer/${KEYMAN_TIER}"
  )

  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function build_developer_action() {
  _build_developer
  _build_testkeyboards
}

function publish_action() {
  if ! builder_is_windows; then
    # requires Powershell, so currently only supported on Windows
    builder_echo error "This script is intended to be run on Windows only."
    return 1
  fi

  export RSYNC_PATH
  export RSYNC_USER
  export RSYNC_HOST
  export RSYNC_ROOT
  export KEYMAN_SYMSTOREPATH="$LOCAL_SYMBOLS_PATH"

  _publish_sentry
  ba_win_download_symbol_server_index
  ba_win_publish_new_symbols
  _publish_to_downloads_keyman_com
  tc_upload_help "api documentation" developer
}

if builder_has_action all; then
  build_developer_action
  publish_action
else
  builder_run_action  build    build_developer_action
  builder_run_action  publish  publish_action
fi
