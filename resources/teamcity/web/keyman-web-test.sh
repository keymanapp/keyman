#!/usr/bin/env bash
# Copyright (C) 2025 SIL International
# Distributed under the MIT License. See LICENSE.md file in the project
# root for full license information.
#
# TC build script for Keyman Web/Test

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-actions.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"

################################ Main script ################################

builder_describe \
  "Run tests for native KeymanWeb" \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          build Web + embedded" \
  "test           run native KeymanWeb tests and check build size"

builder_parse "$@"

cd "${KEYMAN_ROOT}/web"

function install_dependencies_action() {
  builder_echo start "install dependencies" "Install dependencies"

  # shellcheck disable=SC2086
  check_and_install_packages devscripts jq

  # TODO: we can we do something similar for Windows and macOS?
  linux_install_nvm
  install_playwright_dependencies

  builder_echo end "install dependencies" success "Finished installing dependencies"
}

function install_playwright_dependencies() {
  if ! is_ubuntu || ! is_os_version_or_higher 24.04; then
    return 0
  fi

  # shellcheck disable=SC2086
  check_and_install_packages ibevent-2.1-7t64 libxslt1.1 libwoff1 \
    libvpx9 libgstreamer-plugins-bad1.0-0 libwebpdemux2 libharfbuzz-icu0 \
    libenchant-2-2 libsecret-1-0 libhyphen0 libmanette-0.2-0 libflite1 \
    gstreamer1.0-libav libnss3 libnspr4 libatk1.0-0t64 libatk-bridge2.0-0t64 \
    libcups2t64 libatspi2.0-0t64 libxcomposite1 libxdamage1 libxrandr2 \
    libxcursor1 libgtk-3-0t64 libgles2
}

function check_build_size_action() {
  builder_echo start "check build size" "Check build size"
  "${KEYMAN_ROOT}/web/ci.sh" validate-size
  builder_echo end "check build size" success "Finished checking build size"
}

if builder_has_action all; then
  install_dependencies_action

  set_variables_for_nvm

  web_build_action
  web_test_action
  check_build_size_action
else
  builder_run_action  configure   install_dependencies_action

  set_variables_for_nvm

  builder_run_action  build       web_build_action
  builder_run_action  test        web_test_action
  builder_run_action  test        check_build_size_action
fi
