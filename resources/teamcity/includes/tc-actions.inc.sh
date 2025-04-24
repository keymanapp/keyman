#!/usr/bin/env bash

linux_clean_action() {
  builder_heading "Cleaning up"
  # shellcheck disable=SC2154
  "${KEYMAN_ROOT}/linux/build.sh" clean
}

linux_install_dependencies_action() {
  builder_heading "Installing dependencies"
  . "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"
  checkAndInstallRequirements
}

linux_additional_dependencies_action() {
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

linux_build_action() {
  INSTALLDIR="$(mktemp -d)"
  # shellcheck disable=SC2068
  DESTDIR="${INSTALLDIR}" "${KEYMAN_ROOT}/linux/build.sh" clean configure build install $@
}

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

