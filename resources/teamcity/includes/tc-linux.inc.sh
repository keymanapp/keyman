#!/usr/bin/env bash

# Returns 0 if the OS version is greater than or equal to the specified version.
# Parameter:
#   $1 - OS version to compare against (e.g., "20.04")
is_os_version_or_higher() {
  local OS_VERSION=$1

  # we use `dpkg --compare-versions` to compare the current Ubuntu version
  # shellcheck disable=SC2312
  dpkg --compare-versions "$(lsb_release -r -s)" ge "${OS_VERSION}"
}

# Returns 0 if the specified package is installed.
# Parameter:
#   $1 - Package name to check (e.g., "curl")
is_package_installed() {
  local PACKAGE=$1

  dpkg -l "${PACKAGE}" >/dev/null 2>&1
}

set_variables_for_nvm() {
  # nvm.sh uses some variables that might not be initialized, so we
  # disable the "unbound variable" check temporarily
  set -u
  export NVM_DIR="${HOME}/.nvm"
  # shellcheck disable=SC1091
  . "${NVM_DIR}/nvm.sh"
  set +u
  export KEYMAN_USE_NVM=1
  PATH=${HOME}/.keyman/node:${PATH}
}
