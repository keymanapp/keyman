#!/usr/bin/env bash

# Run the clean action in the `linux` directory.`
linux_clean_action() {
  builder_heading "Cleaning up"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/linux/build.sh" clean
}

# Install required dependencies for building Keyman on Linux.
linux_install_dependencies_action() {
  builder_heading "Installing dependencies"
  . "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"
  checkAndInstallRequirements
}

# Install additional dependencies required for determining test coverage.
linux_additional_test_dependencies_action() {
  builder_echo start additional_dependencies "Installing additional dependencies"
  local TOINSTALL="lcov libdatetime-perl gcovr python3-venv jq"

  if is_os_version_or_higher 24.04; then
    TOINSTALL="${TOINSTALL} libgirepository-2.0-dev python3-coverage"
  fi

  # shellcheck disable=SC2086
  check_and_install_packages ${TOINSTALL}

  if ! is_os_version_or_higher 24.04; then
    builder_heading "Installing python3-coverage from pip"
    pip3 install --user coverage
  fi

  linux_install_nvm
  linux_install_dependencies_for_tests

  builder_echo end additional_dependencies success "Finished installing additional dependencies"
}

# Build Keyman for Linux.
linux_build_action() {
  INSTALLDIR="$(mktemp -d)"
  # shellcheck disable=SC2068
  DESTDIR="${INSTALLDIR}" "${KEYMAN_ROOT}/linux/build.sh" clean configure build install $@
}

# Run unit tests for Keyman for Linux.
linux_unit_tests_action() {
  builder_echo start unit_tests "Running unit tests"
  rm -f /tmp/ibus-engine-keyman.log
  rm -f /tmp/ibus-daemon.log
  # symlink might point to wrong location, so delete it - will be re-created during tests
  rm -rf ~/.local/share/keyman/test_kmx

  export NO_AT_BRIDGE=1
  # shellcheck disable=SC2068
  "${KEYMAN_ROOT}/linux/build.sh" test $@
  builder_echo end unit_tests success "Finished running unit tests"
}

web_install_dependencies_action() {
  builder_echo start "install dependencies" "Install dependencies"

  # shellcheck disable=SC2086
  check_and_install_packages devscripts jq

  # TODO: we can we do something similar for Windows and macOS?
  linux_install_nvm
  _install_playwright_dependencies

  builder_echo end "install dependencies" success "Finished installing dependencies"
}

_install_playwright_dependencies() {
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

web_build_action() {
  builder_echo start web_build "Building web"
  "${KEYMAN_ROOT}/web/ci.sh" build
  builder_echo end web_build success "Finished building web"
}

web_test_action() {
  builder_echo start web_test "Running tests for native KeymanWeb"
  if is_ubuntu; then
    linux_start_xvfb
    trap "linux_stop_xvfb" ERR
  fi

  "${KEYMAN_ROOT}/web/ci.sh" test

  if is_ubuntu; then
    linux_stop_xvfb
    trap ERR
  fi

  "${KEYMAN_ROOT}/web/build.sh" coverage

  builder_echo end web_test success "Finished running tests for native KeymanWeb"
}

