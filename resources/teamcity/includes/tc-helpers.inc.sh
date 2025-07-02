#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

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
  ba_linux_install_nvm
}

# Set the environment variables required to use node/nvm and set the
# `KEYMAN_USE_NVM` variable so that the build can automatically install
# the required node version.
set_variables_for_nvm() {
  if [[ -f "${HOME}/.nvm/nvm.sh" ]] && [[ -d "${HOME}/.keyman/node" ]]; then
    # nvm.sh uses some variables that might not be initialized, so we
    # disable the "unbound variable" check temporarily
    set -u
    export NVM_DIR="${HOME}/.nvm"
    # shellcheck disable=SC1091
    . "${NVM_DIR}/nvm.sh"
    set +u
    export KEYMAN_USE_NVM=1
    PATH=${HOME}/.keyman/node:${PATH}
  fi
}

install_emscripten() {
  if ! is_ubuntu; then
    # on Windows and macOS build agents are configured manually
    return 0
  fi
  ba_linux_install_emscripten
}

set_variables_for_emscripten() {
  export EMSCRIPTEN_BASE="${EMSCRIPTEN_BASE:-${HOME}/emsdk/upstream/emscripten}"
  export KEYMAN_USE_EMSDK=1
}

upload_help() {
  local PRODUCT=$1
  local PRODUCT_PATH=$2
  builder_echo start "upload help" "Uploading new ${PRODUCT} help to help.keyman.com"

  (
    # shellcheck disable=SC2164
    cd "${KEYMAN_ROOT}/resources/build"
    "${KEYMAN_ROOT}/resources/build/help-keyman-com.sh" "${PRODUCT_PATH}"
  )

  builder_echo end "upload help" success "Finished uploading new ${PRODUCT} help to help.keyman.com"
}
