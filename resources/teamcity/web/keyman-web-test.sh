#!/bin/bash
# Copyright (C) 2025 SIL International. All rights reserved.
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
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"

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
  if is_windows; then
    return 0
  fi

  builder_echo start install_dependencies "Installing dependencies"

  if [[ "$(lsb_release -s -i)" == "Ubuntu" ]] && dpkg --compare-versions "$(lsb_release -r -s)" ge "24.04"; then
    TOINSTALL=""
      # dependencies for playwright:
    for p in ibevent-2.1-7t64 libxslt1.1 libwoff1 libvpx9 libgstreamer-plugins-bad1.0-0 libwebpdemux2 libharfbuzz-icu0 libenchant-2-2 libsecret-1-0 libhyphen0 libmanette-0.2-0 libflite1 gstreamer1.0-libav
    do
      if ! dpkg -s "${p}" >/dev/null 2>&1; then
        TOINSTALL="${TOINSTALL} ${p}"
      fi
    done
      if [[ -n "${TOINSTALL}" ]]; then
        sudo apt-get update
          sudo DEBIAN_FRONTEND="noninteractive" apt-get install -qy ${TOINSTALL}
      fi
  fi
  builder_echo end install_dependencies success "Finished installing dependencies"
}

function build_web_action() {
  builder_echo start web_build "Building KeymanWeb"

  node ../resources/gosh/gosh.js ./ci.sh build

  builder_echo end web_build success "Finished building KeymanWeb"
}

function test_web_action() {
  builder_echo start web_test "Running KeymanWeb tests"

  if is_windows; then
    node ../resources/gosh/gosh.js ./ci.sh test
  else
    if [[ "$(lsb_release -s -i)" == "Ubuntu" ]]; then
      # On Linux start Xvfb etc
      PID_FILE=/tmp/keymanweb-pids
      echo "Starting Xvfb..."
      Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
      echo "kill -9 $! || true" > "$PID_FILE"
      sleep 1
      echo "Starting Xephyr..."
      DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
      echo "kill -9 $! || true" >> "$PID_FILE"
      sleep 1
      echo "Starting metacity"
      metacity --display=:32 &> /dev/null &
      echo "kill -9 $! || true" >> "$PID_FILE"
      export DISPLAY=:32
    else
      # Don't know what's needed on MacOS...
      touch /tmp/keymanweb-pids
    fi
    node ../resources/gosh/gosh.js ./ci.sh test
    bash /tmp/keymanweb-pids
  fi

  node ../resources/gosh/gosh.js ./build.sh coverage
  builder_echo end web_test success "Finished running KeymanWeb tests"
}

function check_build_size_action() {
  builder_echo start check_build_size "Checking build size"

  node ../resources/gosh/gosh.js ./ci.sh validate-size

  builder_echo end check_build_size success "Finished checking build size"
}

if builder_has_action all; then
  install_dependencies_action
  build_web_action
  test_web_action
  check_build_size_action
else
  builder_run_action  configure   install_dependencies_action
  builder_run_action  build       build_web_action
  builder_run_action  test        test_web_action
  builder_run_action  test        check_build_size_action
fi
