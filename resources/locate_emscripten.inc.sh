# shellcheck shell=bash
# no hashbang for .inc.sh

#
# We don't want to rely on emcc.py being on the path, because Emscripten puts far
# too many things onto the path (in particular for us, node).
#
# The following comment suggests that we don't need emcc.py on the path.
# https://github.com/emscripten-core/emscripten/issues/4848#issuecomment-1097357775
#
# So we try and locate emcc.py in common locations ourselves. The search pattern
# is:
#
# 1. Look for $EMSCRIPTEN_BASE (our primary emscripten variable), which should
#    point to the folder that emcc.py is located in
# 2. Look for $EMCC which should point to the emcc.py executable
# 3. Look for emcc.py on the path
#
locate_emscripten() {
  if [[ -z ${EMSCRIPTEN_BASE+x} ]]; then
    if [[ -z ${EMCC+x} ]]; then
      local EMCC=`which emcc.py`
      [[ -z $EMCC ]] && builder_die "locate_emscripten: Could not locate emscripten (emcc.py) on the path or with \$EMCC or \$EMSCRIPTEN_BASE"
    fi
    [[ -f $EMCC && ! -x $EMCC ]] && builder_die "locate_emscripten: Variable EMCC ($EMCC) points to emcc.py but it is not executable"
    [[ -x $EMCC ]] || builder_die "locate_emscripten: Variable EMCC ($EMCC) does not point to a valid executable emcc.py"
    EMSCRIPTEN_BASE="$(dirname "$EMCC")"
  fi
  [[ -f ${EMSCRIPTEN_BASE}/emcc.py && ! -x ${EMSCRIPTEN_BASE}/emcc.py ]] && builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE ($EMSCRIPTEN_BASE) contains emcc.py but it is not executable"
  [[ -x ${EMSCRIPTEN_BASE}/emcc.py ]] || builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE ($EMSCRIPTEN_BASE) does not point to emcc.py's folder"
}
