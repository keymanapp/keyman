#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of Keyman for Windows.

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
. "${KEYMAN_ROOT}/resources/teamcity/windows/windows-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build and publish Keyman for Windows" \
  "all            run all actions" \
  "build          build Keyman for Windows Windows" \
  "test           run Keyman for Windows tests" \
  "publish        publish release" \
  "--rsync-path=RSYNC_PATH                    rsync path on remote server" \
  "--rsync-user=RSYNC_USER                    rsync user on remote server" \
  "--rsync-host=RSYNC_HOST                    rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT                    rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM          path to help.keyman.com repository" \
  "--symbols-local-path=LOCAL_SYMBOLS_PATH    local path to symbols directory" \
  "--symbols-remote-path=REMOTE_SYMBOLS_PATH  remote path to symbols directory" \
  "--symbols-subdir=SYMBOLS_SUBDIR            subdirectory containing symbols"

builder_parse "$@"

cd "${KEYMAN_ROOT}/windows/src"

if ! builder_is_windows; then
  builder_echo error "This script is intended to be run on Windows only."
  exit 1
fi

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"
  (
    cd "${KEYMAN_ROOT}/windows"

    local UPLOAD_PATH KEYMAN_EXE KEYMAN_DESKTOP_MSI SETUP_EXE SETUP_REDIST_ZIP DEBUG_ZIP
    local FIRSTVOICES_MSI FIRSTVOICES_EXE

    # shellcheck disable=SC2154
    UPLOAD_PATH="upload/${KEYMAN_VERSION}"
    KEYMAN_EXE="keyman-${KEYMAN_VERSION}.exe"
    KEYMAN_DESKTOP_MSI="keymandesktop.msi"
    SETUP_EXE="setup.exe"
    SETUP_REDIST_ZIP="setup-redist.zip"
    FIRSTVOICES_MSI="firstvoices.msi"
    FIRSTVOICES_EXE="firstvoices-${KEYMAN_VERSION}.exe"
    DEBUG_ZIP="debug-${KEYMAN_VERSION}.zip"

    rm -rf "${UPLOAD_PATH}"
    mkdir -p "${UPLOAD_PATH}"

    cp "release/${KEYMAN_VERSION}/${KEYMAN_EXE}" "${UPLOAD_PATH}"
    cp "release/${KEYMAN_VERSION}/${KEYMAN_DESKTOP_MSI}" "${UPLOAD_PATH}"
    cp "release/${KEYMAN_VERSION}/${SETUP_EXE}" "${UPLOAD_PATH}"
    cp "release/${KEYMAN_VERSION}/${SETUP_REDIST_ZIP}" "${UPLOAD_PATH}"
    cp "release/${KEYMAN_VERSION}/${FIRSTVOICES_EXE}" "${UPLOAD_PATH}"
    cp "release/${KEYMAN_VERSION}/${FIRSTVOICES_MSI}" "${UPLOAD_PATH}"

    write_download_info "${UPLOAD_PATH}" "${KEYMAN_EXE}" "Keyman for Windows" exe win
    write_download_info "${UPLOAD_PATH}" "${KEYMAN_DESKTOP_MSI}" "Keyman for Windows MSI installer" msi win
    write_download_info "${UPLOAD_PATH}" "${SETUP_EXE}" "Keyman for Windows setup bootstrap" exe win
    write_download_info "${UPLOAD_PATH}" "${SETUP_REDIST_ZIP}" "Keyman for Windows setup bootstrap (unsigned for bundling)" zip win

    write_download_info "${UPLOAD_PATH}" "${FIRSTVOICES_EXE}" "FirstVoices Keyboards" exe win
    write_download_info "${UPLOAD_PATH}" "${FIRSTVOICES_MSI}" "FirstVoices Keyboards MSI installer" msi win

    # TODO: is this still needed?
    if [[ -f "release/${KEYMAN_VERSION}/${DEBUG_ZIP}" ]]; then
      cp "release/${KEYMAN_VERSION}/${DEBUG_ZIP}" "${UPLOAD_PATH}"
      write_download_info "${UPLOAD_PATH}" "${DEBUG_ZIP}" "Keyman for Windows and Keyman Developer debug files" zip win
    fi

    cd upload
    # shellcheck disable=SC2154
    tc_rsync_upload "${KEYMAN_VERSION}" "windows/${KEYMAN_TIER}"
  )
  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function windows_publish_action() {
  builder_echo start "publish windows" "Publishing Keyman for Windows"

  export RSYNC_PATH
  export RSYNC_USER
  export RSYNC_HOST
  export RSYNC_ROOT

  builder_launch /windows/build.sh publish
  windows_upload_symbols_to_sentry
  ba_win_download_symbol_server_index
  ba_win_publish_new_symbols
  _publish_to_downloads_keyman_com
  tc_upload_help "Keyman for Windows" windows
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

