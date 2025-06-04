#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build release of KeymanWeb.

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/web/web-actions.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"

################################ Main script ################################

builder_describe \
  "Run tests for native KeymanWeb" \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          build Web + embedded" \
  "publish        publish release" \
  "--s.keyman.com=S_KEYMAN_COM        path to s.keyman.com repository" \
  "--help.keyman.com=HELP_KEYMAN_COM  path to help.keyman.com repository"

builder_parse "$@"

cd "${KEYMAN_ROOT}/web"

function _push_release_to_skeymancom() {
  # Push release to s.keyman.com/kmw/engine (do this before updating
  # downloads.keyman.com so we can ensure files are available)
  builder_echo start publish "Publishing release to s.keyman.com"

  cd "${S_KEYMAN_COM:=${KEYMAN_ROOT}/../s.keyman.com}"
  git pull https://github.com/keymanapp/s.keyman.com.git master
  cd "${KEYMAN_ROOT}/web"
  "${KEYMAN_ROOT}/web/ci.sh" prepare:s.keyman.com --s.keyman.com "${S_KEYMAN_COM}"

  builder_echo end publish success "Finished publishing release to s.keyman.com"
}

function _zip_and_upload_artifacts() {
  if ! is_windows; then
    # requires Powershell
    return 0
  fi

  builder_echo start "zip and upload artifacts" "Zipping and uploading artifacts"

  cd "${KEYMAN_ROOT}/resources/teamcity/web"
  powershell -NonInteractive -ExecutionPolicy Bypass -File zip-and-upload-artifacts.ps1
  cd "${KEYMAN_ROOT}/web"

  builder_echo end "zip and upload artifacts" success "Finished zipping and uploading artifacts"
}

function _upload_help() {
  builder_echo start "upload help" "Uploading new Keyman for Web help to help.keyman.com"

  export HELP_KEYMAN_COM="${HELP_KEYMAN_COM:-${KEYMAN_ROOT}/../help.keyman.com}"
  cd "${KEYMAN_ROOT}/resources/build"
  "${KEYMAN_ROOT}/resources/build/help-keyman-com.sh" web
  cd "${KEYMAN_ROOT}/web"

  builder_echo end "upload help" success "Finished uploading new Keyman for Web help to help.keyman.com"
}

function publish_web_action() {
  builder_echo start publish "Publishing KeymanWeb release"

  # TODO: refactor to allow to run on Linux/macOS as well
  if ! is_windows; then
    builder_echo end publish error "Publishing KeymanWeb is only supported on Windows"
    return 1
  fi

  # Push release to s.keyman.com/kmw/engine (do this before updating
  # downloads.keyman.com so we can ensure files are available)
  _push_release_to_skeymancom

  _zip_and_upload_artifacts
  _upload_help

  builder_echo end publish success "Finished publishing KeymanWeb release"
}

if builder_has_action all; then
  web_install_dependencies_on_linux_action

  set_variables_for_nvm

  web_build_action
  publish_web_action
else
  builder_run_action  configure   web_install_dependencies_on_linux_action

  set_variables_for_nvm

  builder_run_action  build       web_build_action
  builder_run_action  publish     publish_web_action
fi
