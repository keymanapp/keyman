#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of Keyman Developer

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"

################################ Main script ################################

builder_describe \
  "Build Keyman Developer on Windows" \
  "all            run all actions" \
  "build          build Keyman Developer and test keyboards" \
  "publish        publish release of Keyman Developer" \
  "--rsync-path=RSYNC_PATH            rsync path on remote server" \
  "--rsync-user=RSYNC_USER            rsync user on remote server" \
  "--rsync-host=RSYNC_HOST            rsync host on remote server" \
  "--rsync-root=RSYNC_ROOT            rsync root on remote server" \
  "--help.keyman.com=HELP_KEYMAN_COM  path to help.keyman.com repository"

builder_parse "$@"

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/developer/src"

function _build_developer() {
  builder_echo start "build developer" "Building Keyman Developer"

  ./build.sh configure build test api publish --npm-publish

  builder_echo end "build developer" success "Finished building Keyman Developer"
}

function _build_testkeyboards() {
  builder_echo start "build testkeyboards" "Building test keyboards"

  "${KEYMAN_ROOT}/common/test/keyboards/build.sh"

  builder_echo end "build testkeyboards" success "Finished building test keyboards"
}

function _publish_sentry() {
  builder_echo start "publish sentry" "Publishing debug information files to Sentry"

  "${KEYMAN_ROOT}/developer/src/tools/sentry-upload-difs.sh"

  builder_echo end "publish sentry" success "Finished publishing debug information files to Sentry"
}

function _download_symbol_server_index() {
  # Download symbol server index from symbol server
  builder_echo start "download symbol server index" "Downloading symbol server index"

  cd "${KEYMAN_ROOT}/.."
  # shellcheck disable=SC2154
  powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/download-symbol-server-index.ps1"
  cd "${KEYMAN_ROOT}/developer/src"

  builder_echo end "download symbol server index" success "Finished downloading symbol server index"
}

function _publish_new_symbols() {
  # Publish new symbols to symbol server
  builder_echo start "publish new symbols" "Publishing new symbols to symbol server"

  cd "${KEYMAN_ROOT}/../symbols"
  # shellcheck disable=SC2154
  powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/publish-new-symbols.ps1"
  cd "${KEYMAN_ROOT}/developer/src"

  builder_echo end "publish new symbols" success "Finished publishing new symbols to symbol server"
}

function _publish_to_downloads_keyman_com() {
  # Publish to downloads.keyman.com
  builder_echo start "publish to downloads.keyman.com" "Publishing release to downloads.keyman.com"

  cd "${KEYMAN_ROOT}/developer"
  # shellcheck disable=SC2154
  powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/publish-to-downloads-keyman-com.ps1"
  cd "${KEYMAN_ROOT}/developer/src"

  builder_echo end "publish to downloads.keyman.com" success "Finished publishing release to downloads.keyman.com"
}

function _publish_api_documentation() {
  # Upload new Keyman Developer API documentation to help.keyman.com
  builder_echo start "publish api documentation" "Uploading new Keyman Developer API documentation to help.keyman.com"

  export HELP_KEYMAN_COM="${HELP_KEYMAN_COM:-${KEYMAN_ROOT}/../help.keyman.com}"
  "${KEYMAN_ROOT}/resources/build/help-keyman-com.sh" developer

  builder_echo end "publish api documentation" success "Finished uploading new Keyman Developer API documentation to help.keyman.com"
}

function build_developer_action() {
  _build_developer
  _build_testkeyboards
}

function publish_action() {
  if ! is_windows; then
    # requires Powershell, so currently only supported on Windows
    builder_echo error "This script is intended to be run on Windows only."
    return 1
  fi

  export RSYNC_PATH
  export RSYNC_USER
  export RSYNC_HOST
  export RSYNC_ROOT

  _publish_sentry
  _download_symbol_server_index
  _publish_new_symbols
  _publish_to_downloads_keyman_com
  _publish_api_documentation
}

if builder_has_action all; then
  build_developer_action
  publish_action
else
  builder_run_action  build    build_developer_action
  builder_run_action  publish  publish_action
fi
