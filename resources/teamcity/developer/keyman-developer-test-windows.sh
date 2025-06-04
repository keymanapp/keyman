#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman Developer on Windows

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build Keyman Developer on Windows" \
  "all            run all actions" \
  "build          build Keyman Developer and test keyboards" \
  "publish        publish debug information files to sentry"

builder_parse "$@"

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/developer/src"

function build_developer_action() {
  _build_developer
  _build_testkeyboards
}

function _build_developer() {
  builder_echo start "build developer" "Building Keyman Developer"

  ./build.sh configure build test api publish --dry-run

  builder_echo end "build developer" success "Finished building Keyman Developer"
}

function _build_testkeyboards() {
  builder_echo start "build testkeyboards" "Building test keyboards"

  "${KEYMAN_ROOT}/common/test/keyboards/build.sh" --zip-source --index

  builder_echo end "build testkeyboards" success "Finished building test keyboards"
}

function publish_sentry_action() {
  builder_echo start "publish sentry" "Publishing debug information files to Sentry"

  "${KEYMAN_ROOT}/developer/src/tools/sentry-upload-difs.sh"

  builder_echo end "publish sentry" success "Finished publishing debug information files to Sentry"
}

if ! is_windows; then
  builder_echo error "This script is intended to be run on Windows only."
  exit 1
fi

if builder_has_action all; then
  build_developer_action
  publish_sentry_action
else
  builder_run_action  build    build_developer_action
  builder_run_action  publish  publish_sentry_action
fi
