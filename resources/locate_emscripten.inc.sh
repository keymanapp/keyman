# shellcheck shell=bash
# no hashbang for .inc.sh

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
  if [[ "${BUILDER_OS}" == "win" ]]; then
    EMCC_EXECUTABLE="emcc.py"
  else
    EMCC_EXECUTABLE="emcc"
  fi
  if [[ -z ${EMSCRIPTEN_BASE+x} ]]; then
    if [[ -z ${EMCC+x} ]]; then
      local EMCC=$(which ${EMCC_EXECUTABLE})
      [[ -z $EMCC ]] && builder_die "locate_emscripten: Could not locate emscripten (${EMCC_EXECUTABLE}) on the path or with \$EMCC or \$EMSCRIPTEN_BASE"
    fi
    [[ -f $EMCC && ! -x $EMCC ]] && builder_die "locate_emscripten: Variable EMCC ($EMCC) points to ${EMCC_EXECUTABLE} but it is not executable"
    [[ -x $EMCC ]] || builder_die "locate_emscripten: Variable EMCC ($EMCC) does not point to a valid executable ${EMCC_EXECUTABLE}"
    EMSCRIPTEN_BASE="$(dirname "$EMCC")"
  fi
  [[ -f ${EMSCRIPTEN_BASE}/${EMCC_EXECUTABLE} && ! -x ${EMSCRIPTEN_BASE}/${EMCC_EXECUTABLE} ]] && builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE ($EMSCRIPTEN_BASE) contains ${EMCC_EXECUTABLE} but it is not executable"
  [[ -x ${EMSCRIPTEN_BASE}/${EMCC_EXECUTABLE} ]] || builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE ($EMSCRIPTEN_BASE) does not point to ${EMCC_EXECUTABLE}'s folder"
}
