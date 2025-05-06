#!/usr/bin/env bash

# Returns 0 if the OS version is greater than or equal to the specified version.
# Parameter:
#   $1 - OS version to compare against (e.g., "20.04")
is_os_version_or_higher() {
  OS_VERSION=$1

  # we use `dpkg --compare-versions` to compare the current Ubuntu version
  # shellcheck disable=SC2312
  dpkg --compare-versions "$(lsb_release -r -s)" ge "${OS_VERSION}"
}

# Returns 0 if the specified package is installed.
# Parameter:
#   $1 - Package name to check (e.g., "curl")
is_package_installed() {
  PACKAGE=$1

  dpkg -l "${PACKAGE}" >/dev/null 2>&1
}

install_dependencies_action() {
  builder_heading "Installing dependencies"
  # shellcheck disable=SC2154
  . "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"
  checkAndInstallRequirements
}
