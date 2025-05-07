#!/usr/bin/env bash

linux_install_dependencies_action() {
  builder_heading "Installing dependencies"
  # shellcheck disable=SC2154
  . "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"
  checkAndInstallRequirements
}
