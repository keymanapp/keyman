#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

# Run the clean action in the `linux` directory.`
linux_clean_action() {
  builder_heading "Cleaning up"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/linux/build.sh" clean
}

# Install required dependencies for building Keyman on Linux.
linux_install_dependencies_action() {
  builder_echo start "install dependencies" "Installing dependencies"
  . "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"
  checkAndInstallRequirements
  builder_echo end "install dependencies" success "Finished installing dependencies"
}

# Install additional dependencies required for determining test coverage.
linux_additional_test_dependencies_action() {
  builder_echo start additional_dependencies "Installing additional dependencies"
  local TOINSTALL="lcov libdatetime-perl gcovr python3-venv jq"

  if is_os_version_or_higher 24.04; then
    TOINSTALL="${TOINSTALL} libgirepository-2.0-dev python3-coverage"
  fi

  # shellcheck disable=SC2086
  ba_linux_check_and_install_packages ${TOINSTALL}

  if ! is_os_version_or_higher 24.04; then
    builder_heading "Installing python3-coverage from pip"
    pip3 install --user coverage
  fi

  ba_linux_install_nvm
  ba_linux_install_dependencies_for_tests

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
  builder_echo startTest unit_tests "Running unit tests"

  rm -f /tmp/ibus-engine-keyman.log
  rm -f /tmp/ibus-daemon.log
  # symlink might point to wrong location, so delete it - will be re-created during tests
  rm -rf ~/.local/share/keyman/test_kmx

  export NO_AT_BRIDGE=1
  # shellcheck disable=SC2068
  "${KEYMAN_ROOT}/linux/build.sh" test $@
  builder_echo endTest unit_tests success "Finished running unit tests"
}
