#!/usr/bin/env bash

# Returns 0 if we're running on Ubuntu.
is_ubuntu() {
  if [[ "${OSTYPE:-}" == "linux-gnu" ]]; then
    return 0
  else
    return 1
  fi
}

# Returns 0 if we're running on Windows, i.e. if the environment variable
# `OSTYPE` is set to "msys" or "cygwin".
is_windows() {
  if [[ "${OSTYPE:-}" == "msys" ]] || [[ "${OSTYPE:-}" == "cygwin" ]]; then
    return 0
  else
    return 1
  fi
}

# Returns 0 if we're running on macOS.
is_macos() {
  if [[ "${OSTYPE:-}" == "darwin" ]]; then
    return 0
  else
    return 1
  fi
}

install_nvm() {
  if ! is_ubuntu; then
    # on Windows and macOS build agents are configured manually
    return 0
  fi
  linux_install_nvm
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

install_emscripten() {
  if ! is_ubuntu; then
    # on Windows and macOS build agents are configured manually
    return 0
  fi
  linux_install_emscripten
}

set_variables_for_emscripten() {
  export EMSCRIPTEN_BASE="${EMSCRIPTEN_BASE:-${HOME}/emsdk/upstream/emscripten}"
  export KEYMAN_USE_EMSDK=1
}
