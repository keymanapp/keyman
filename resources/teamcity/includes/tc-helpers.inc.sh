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
    # shellcheck disable=SC2164,SC2154
    cd "${KEYMAN_ROOT}/resources/build"
    "${KEYMAN_ROOT}/resources/build/help-keyman-com.sh" "${PRODUCT_PATH}"
  )

  builder_echo end "upload help" success "Finished uploading new ${PRODUCT} help to help.keyman.com"
}

_tc_rsync() {
  local rsync_args CYGPATH_RSYNC_HOME SOURCE DESTINATION
  SOURCE=$1
  DESTINATION=$2

  if ! is_windows; then
    echo "This function is currently only supported on Windows."
    return 1
  fi

  rsync_args=(
    '-vrzltp'                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
    '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r'      # map Windows security to host security
    '--stats'                                # show statistics for log
    "--rsync-path=${RSYNC_PATH}"             # path on remote server
    "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts"                  # use ssh
    "${SOURCE}"
    "${DESTINATION}"
  )

  CYGPATH_RSYNC_HOME=$(cygpath -w "${RSYNC_HOME}")
  MSYS_NO_PATHCONV=1 "${CYGPATH_RSYNC_HOME}\\rsync.exe" "${rsync_args[@]}"
}

# Download file or directory $1 from the remote server to $2.
tc_rsync_download() {
  # shellcheck disable=SC2154
  _tc_rsync \
    "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/$1" \
    "$2"
}

# Upload file or directory $1 to the remote server at $2.
tc_rsync_upload() {
  _tc_rsync \
    "$1" \
    "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/$2"
}
