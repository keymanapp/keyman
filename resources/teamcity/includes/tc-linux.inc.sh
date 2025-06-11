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
linux_check_and_install_packages() {
  if ! is_ubuntu; then
    return 0
  fi

  builder_echo start "check and install packages" "Checking and installing packages"
  local PACKAGES=$*
  local TOINSTALL=""

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
  builder_echo end "check and install packages" success "Finished checking and installing packages"
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

linux_install_emscripten() {
  builder_echo start "install emscripten" "Checking and installing emscripten"
  if { [[ ! -z "${EMSCRIPTEN_BASE:-}" ]] && [[ -f "${EMSCRIPTEN_BASE}/emcc" ]] ; } ||
    [[ -f "${HOME}/emsdk/upstream/emscripten/emcc" ]]; then
    builder_echo "Emscripten is already installed at ${EMSCRIPTEN_BASE:-${HOME}/emsdk/upstream/emscripten}"
  else
    # shellcheck disable=SC2154
    . "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"

    builder_echo "Installing emscripten version ${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
    export EMSDK_KEEP_DOWNLOADS=1
    # shellcheck disable=SC2164
    cd "${HOME}"
    git clone https://github.com/emscripten-core/emsdk.git
    # shellcheck disable=SC2164
    cd emsdk
    ./emsdk install "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
    ./emsdk activate "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
  fi
  set_variables_for_emscripten
  builder_echo end "install emscripten" success "Finished checking and installing emscripten"
}

# Install additional dependencies required for running integration tests.
linux_install_dependencies_for_tests() {
  builder_echo start install_dependencies_for_tests "Installing dependencies for tests"

  linux_check_and_install_packages xvfb xserver-xephyr metacity mutter dbus-x11 weston xwayland

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
