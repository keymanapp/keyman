#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman Developer on Windows

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"

################################ Main script ################################

builder_describe \
  "Build Keyman Developer on Windows" \
  "all            run all actions (used by TeamCity build configuration)" \
  "build          build Keyman Developer and test keyboards" \
  "publish        publish debug information files to sentry"

builder_parse "$@"

if ! builder_is_windows; then
  builder_echo error "This script is intended to be run on Windows only."
  exit 1
fi

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/developer/src"

function build_developer_action() {
  _build_developer
  _build_testkeyboards
}

function _build_developer() {
  builder_echo start "build developer" "Building and testing Keyman Developer"

  builder_launch /developer/src/build.sh configure build test

  builder_echo end "build developer" success "Finished building and testing Keyman Developer"
}

function _build_testkeyboards() {
  builder_echo start "build testkeyboards" "Building test keyboards"

  builder_launch /common/test/keyboards/build.sh --zip-source --index

  builder_echo end "build testkeyboards" success "Finished building test keyboards"
}

function publish_sentry_action() {
  builder_echo start "publish" "publish modules locally and prep api"
  builder_launch /developer/src/build.sh api publish
  builder_echo end "publish" "publish modules locally and prep api"

  builder_echo start "publish sentry" "Publishing debug information files to Sentry"
  # TODO: move this into build.sh publish?
  builder_launch /developer/src/tools/sentry-upload-difs.sh
  builder_echo end "publish sentry" success "Finished publishing debug information files to Sentry"
}

if builder_has_action all; then
  # TODO: control codesign by KEYMAN_BUILD_LEVEL
  build_developer_action
  if builder_is_ci_build_level_release; then
    publish_sentry_action
  fi
else
  builder_run_action  build    build_developer_action
  builder_run_action  publish  publish_sentry_action
fi
