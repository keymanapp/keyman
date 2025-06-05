#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

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

  dpkg -s "${PACKAGE}" >/dev/null 2>&1
}

# Check if the specified packages are installed and install them if not.
# Parameters:
#   $* - List of package names to check and install (e.g., "lcov jq")
check_and_install_packages() {
  local PACKAGES=$*
  local TOINSTALL=""

  if ! is_ubuntu; then
    return 0
  fi

  for p in ${PACKAGES}
  do
    if ! is_package_installed "${p}"; then
      TOINSTALL="${TOINSTALL} ${p}"
    fi
  done

  if [[ -n "${TOINSTALL}" ]]; then
    sudo apt-get update
    # shellcheck disable=SC2086
    sudo DEBIAN_FRONTEND="noninteractive" apt-get install -qy ${TOINSTALL}
  fi
}

# Set the environment variables required to use node/nvm and set the
# `KEYMAN_USE_NVM` variable so that the build can automatically install
# the required node version.
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

# Install nvm if it is not already installed and install the latest LTS
linux_install_nvm() {
  builder_echo start install_nvm "Checking and installing nvm"
  if [[ -f "${HOME}/.nvm/nvm.sh" ]]; then
    builder_echo "nvm is already installed"
  else
    # Install nvm
    # shellcheck disable=SC2312
    NVM_RELEASE=$(curl -s https://api.github.com/repos/nvm-sh/nvm/releases/latest | grep tag_name | cut -d : -f 2 | cut -d '"' -f 2)
    # shellcheck disable=SC2312
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_RELEASE}/install.sh | bash
    set_variables_for_nvm
    nvm install --lts --default --save
    nvm use --lts
  fi
  builder_echo end install_nvm success "Finished checking and installing nvm"
}

# Install additional dependencies required for running integration tests.
linux_install_dependencies_for_tests() {
  builder_echo start install_dependencies_for_tests "Installing dependencies for tests"

  check_and_install_packages xvfb xserver-xephyr metacity mutter dbus-x11 weston xwayland

  builder_echo end install_dependencies_for_tests success "Finished installing dependencies for tests"
}

linux_start_xvfb() {
  # On Linux start Xvfb etc
  local PID_FILE=/tmp/keymanweb-pids
  builder_echo "Starting Xvfb..."
  Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
  echo "kill -9 $! || true" > "${PID_FILE}"
  sleep 1
  builder_echo "Starting Xephyr..."
  DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
  echo "kill -9 $! || true" >> "${PID_FILE}"
  sleep 1
  builder_echo "Starting metacity"
  metacity --display=:32 &> /dev/null &
  echo "kill -9 $! || true" >> "${PID_FILE}"
  export DISPLAY=:32
}

linux_stop_xvfb() {
  # On Linux stop Xvfb etc
  local PID_FILE=/tmp/keymanweb-pids
  if [[ -f "${PID_FILE}" ]]; then
    bash "${PID_FILE}"
    rm -f "${PID_FILE}"
  fi
}
