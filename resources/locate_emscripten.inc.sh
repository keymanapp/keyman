# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"

#
# We don't want to rely on emcc being on the path, because Emscripten puts far
# too many things onto the path (in particular for us, node).
#
# The following comment suggests that we don't need emcc on the path.
# https://github.com/emscripten-core/emscripten/issues/4848#issuecomment-1097357775
#
# So we try and locate emcc in common locations ourselves. The search pattern
# is:
#
# 1. Look for $EMSCRIPTEN_BASE (our primary emscripten variable), which should
#    point to the folder that emcc is located in
# 2. Look for $EMCC which should point to the emcc executable
# 3. Look for emcc on the path
#
locate_emscripten() {
  local EMCC_EXECUTABLE
  if builder_is_windows; then
    EMCC_EXECUTABLE="emcc.py"
  else
    EMCC_EXECUTABLE="emcc"
  fi
  if [[ -z "${EMSCRIPTEN_BASE:-}" ]]; then
    if [[ -z "${EMCC:-}" ]]; then
      local EMCC
      EMCC="$(command -v "${EMCC_EXECUTABLE}")"
      [[ -z "${EMCC}" ]] && builder_die "locate_emscripten: Could not locate emscripten (${EMCC_EXECUTABLE}) on the path or with \$EMCC or \$EMSCRIPTEN_BASE"
    fi
    [[ -f "${EMCC}" && ! -x "${EMCC}" ]] && builder_die "locate_emscripten: Variable EMCC (${EMCC}) points to ${EMCC_EXECUTABLE} but it is not executable"
    [[ -x "${EMCC}" ]] || builder_die "locate_emscripten: Variable EMCC (${EMCC}) does not point to a valid executable ${EMCC_EXECUTABLE}"
    EMSCRIPTEN_BASE="$(dirname "${EMCC}")"
  fi
  [[ -f "${EMSCRIPTEN_BASE}/${EMCC_EXECUTABLE}" && ! -x "${EMSCRIPTEN_BASE}/${EMCC_EXECUTABLE}" ]] && builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE (${EMSCRIPTEN_BASE}) contains ${EMCC_EXECUTABLE} but it is not executable"
  if [[ ! -d "${EMSCRIPTEN_BASE}" ]]; then
    # $EMSCRIPTEN_BASE does not exist, but check if we have emsdk in a parent directory. #13464
    _can_locate_emsdk "${EMSCRIPTEN_BASE}" || builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE (${EMSCRIPTEN_BASE}) points to a non-existent directory"
  else
    [[ -x "${EMSCRIPTEN_BASE}/${EMCC_EXECUTABLE}" ]] || builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE (${EMSCRIPTEN_BASE}) does not point to ${EMCC_EXECUTABLE}'s folder"
  fi
  verify_emscripten_version
}

# Ensure that we use correct version of emsdk on build agents.
# For developers, define KEYMAN_USE_EMSDK to do this on your
# build machine.
verify_emscripten_version() {
  # shellcheck disable=SC2154
  if [[ "${KEYMAN_VERSION_ENVIRONMENT}" != local || ! -z "${KEYMAN_USE_EMSDK+x}" ]]; then
    _select_emscripten_version_with_emsdk
  fi
}

# Check if the grandparent directory (i.e. emscripten root) contains emsdk
_can_locate_emsdk() {
  local DIR="$1"
  local GRANDPARENT
  GRANDPARENT="$(dirname "$(dirname "${DIR}")")"

  if [[ -f "${GRANDPARENT}/emsdk" ]]; then
    return 0
  else
    return 1
  fi
}

# Use emsdk to select the appropriate version of Emscripten
# according to minimum-versions.inc.sh
_select_emscripten_version_with_emsdk() {
  if [[ -z "${EMSCRIPTEN_BASE+x}" ]]; then
    builder_die "Variable EMSCRIPTEN_BASE must be set"
  fi

  if [[ -z "${KEYMAN_MIN_VERSION_EMSCRIPTEN+x}" ]]; then
    builder_die "Variable KEYMAN_MIN_VERSION_EMSCRIPTEN must be set"
  fi

  local GRANDPARENT
  GRANDPARENT="$(dirname "$(dirname "${EMSCRIPTEN_BASE}")")"
  (
    # shellcheck disable=SC2164
    cd "${GRANDPARENT}"
    if [[ ! -f emsdk ]]; then
      builder_die "emsdk[.bat] should be in ${GRANDPARENT}"
    fi

    export EMSDK_KEEP_DOWNLOADS=1
    if builder_try_offline; then
      if [[ ! -f ./emsdk_env.sh ]]; then
        builder_die "emsdk_env.sh not found - emsdk not installed?"
      fi
      . ./emsdk_env.sh
      if ! emcc --version | grep -q "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"; then
        builder_die "Wrong emsdk version installed"
      fi
    else
      git pull
      ./emsdk install "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
      ./emsdk activate "${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
      # shellcheck disable=SC2164
      cd upstream/emscripten
      npm install
    fi
  )
}
