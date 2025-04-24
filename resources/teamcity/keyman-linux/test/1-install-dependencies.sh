#!/usr/bin/env bash
# Step name: Install dependencies
#
# Expected to be run from Keyman repo root directory
# shellcheck disable=SC2310

set -eu

. "resources/teamcity/keyman-linux/includes/helperFunctions.inc.sh"

log "Installing dependencies"
. linux/scripts/package-build.inc.sh
checkAndInstallRequirements

log "Installing additional dependencies"
export DEBIAN_FRONTEND=noninteractive

TOINSTALL=""
for p in lcov libdatetime-perl gcovr python3-venv jq
do
  if ! is_package_installed "${p}"; then
    TOINSTALL="${TOINSTALL} ${p}"
  fi
done

if is_os_version_or_higher 24.04; then
  for p in libgirepository-2.0-dev python3-coverage
  do
    if ! is_package_installed "${p}"; then
      TOINSTALL="${TOINSTALL} ${p}"
    fi
  done
fi

if [[ -n "${TOINSTALL}" ]]; then
  sudo apt-get update
  sudo apt-get install -qy ${TOINSTALL}
fi

if ! is_os_version_or_higher 24.04; then
  log "Installing python3-coverage from pip"
  pip3 install --user coverage
fi

log "Checking and installing nvm"
if [[ ! -f "${HOME}/.nvm/nvm.sh" ]]; then
  # Install nvm
  NVM_RELEASE=$(curl -s https://api.github.com/repos/nvm-sh/nvm/releases/latest | grep tag_name | cut -d : -f 2 | cut -d '"' -f 2)
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_RELEASE}/install.sh | bash
  . "${HOME}/.nvm/nvm.sh"
  nvm install --lts
  nvm use --lts
fi
