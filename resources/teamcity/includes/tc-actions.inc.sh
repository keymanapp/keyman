#!/usr/bin/env bash

linux_install_dependencies_action() {
  builder_heading "Installing dependencies"
  # shellcheck disable=SC2154
  . "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"
  checkAndInstallRequirements
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
  builder_echo end unit_tests success "Running unit tests"
}

