# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

# Set the environment variables required to use node/nvm and set the
# `KEYMAN_USE_NVM` variable so that the build can automatically install
# the required node version.
tc_set_variables_for_nvm() {
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

tc_set_variables_for_emscripten() {
  export EMSCRIPTEN_BASE="${EMSCRIPTEN_BASE:-${HOME}/emsdk/upstream/emscripten}"
  export KEYMAN_USE_EMSDK=1
}

tc_upload_help() {
  local PRODUCT=$1
  local PRODUCT_PATH=$2
  builder_echo start "upload help" "Uploading new ${PRODUCT} help to help.keyman.com"
  "${KEYMAN_ROOT}/resources/build/ci/help-keyman-com.sh" "${PRODUCT_PATH}"
  builder_echo end "upload help" success "Finished uploading new ${PRODUCT} help to help.keyman.com"
}

_tc_rsync() {
  local rsync_args SOURCE DESTINATION
  SOURCE=$1
  DESTINATION=$2

  rsync_args=(
    '-vrzltp'                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
    '--stats'                                # show statistics for log
    "--rsync-path=${RSYNC_PATH}"             # path on remote server
  )

  if builder_is_windows; then
    rsync_args+=(
      '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r'      # map Windows security to host security
      "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts"                  # use ssh
    )
  else
    rsync_args+=(
      "--rsh=ssh"                          # use ssh
    )
  fi

  rsync_args+=(
      "${SOURCE}"
      "${DESTINATION}"
  )

  if builder_is_windows; then
    local CYGPATH_RSYNC_HOME
    CYGPATH_RSYNC_HOME=$(cygpath -w "${RSYNC_HOME}")
    MSYS_NO_PATHCONV=1 "${CYGPATH_RSYNC_HOME}\\rsync.exe" "${rsync_args[@]}"
  else
    local RSYNC=rsync
    if builder_is_macos; then
      RSYNC=/usr/local/bin/rsync
      [[ -f /opt/homebrew/bin/rsync ]] && RSYNC=/opt/homebrew/bin/rsync
    fi

    ${RSYNC} "${rsync_args[@]}"
  fi
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
