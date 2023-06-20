#!/usr/bin/env bash

# ----------------------------------------------------------------------------
# clean
# ----------------------------------------------------------------------------

do_clean() {
  # clean: note build/<target> will be left, but build/<target>/<configuration> should be gone
  local target=$1
  builder_start_action clean:$target || return 0

  rm -rf "$MESON_PATH"
  builder_finish_action success clean:$target
}

# ----------------------------------------------------------------------------
# configure
# ----------------------------------------------------------------------------

do_configure() {
  local target=$1
  builder_start_action configure:$target || return 0

  local STANDARD_MESON_ARGS=

  if [[ $target == wasm ]]; then
    # do_configure_wasm
    locate_emscripten
    build_meson_cross_file_for_wasm
    STANDARD_MESON_ARGS="--cross-file wasm.defs.build --cross-file wasm.build --default-library static"
  fi

  if [[ $target =~ ^(x86|x64)$ ]]; then
    cmd //C build.bat $target $BUILDER_CONFIGURATION configure "${builder_extra_params[@]}"
  else
    pushd "$THIS_SCRIPT_PATH" > /dev/null
    # Additional arguments are used by Linux build, e.g. -Dprefix=${INSTALLDIR}
    meson setup "$MESON_PATH" --werror --buildtype $BUILDER_CONFIGURATION $STANDARD_MESON_ARGS "${builder_extra_params[@]}"
    popd > /dev/null
  fi
  builder_finish_action success configure:$target
}

# ----------------------------------------------------------------------------
# build
# ----------------------------------------------------------------------------

do_build() {
  local target=$1
  builder_start_action build:$target || return 0

  if [[ $target =~ ^(x86|x64)$ ]]; then
    # Build the meson targets, both x86 and x64 also
    # We need to use a batch file here so we can get
    # the Visual Studio build environment with vcvarsall.bat
    # TODO: if PATH is the only variable required, let's try and
    #       eliminate this difference in the build process
    cmd //C build.bat $target $BUILDER_CONFIGURATION build "${builder_extra_params[@]}"
  else
    pushd "$MESON_PATH" > /dev/null
    ninja
    popd > /dev/null
  fi
  builder_finish_action success build:$target
}

# ----------------------------------------------------------------------------
# test
# ----------------------------------------------------------------------------

do_test() {
  local target=$1
  builder_start_action test:$target || return 0

  # Works on a local clone of keyboards repository, to avoid clobbering
  # user's existing keyboards repo, if present

  if builder_has_option --full-test; then
    checkout_keyboards
  fi

  if [[ $target =~ ^(x86|x64)$ ]]; then
    cmd //C build.bat $target $BUILDER_CONFIGURATION test "${builder_extra_params[@]}"
  else
    pushd "$MESON_PATH" > /dev/null
    meson test "${builder_extra_params[@]}"
    popd > /dev/null
  fi
  builder_finish_action success test:$target
}

# ----------------------------------------------------------------------------
# install and uninstall
# ----------------------------------------------------------------------------

do_install() {
  do_command install $1
}

do_uninstall() {
  do_command uninstall $1
}

do_command() {
  local command=$1
  local target=$2
  builder_start_action $command:$target || return 0
  pushd "$MESON_PATH" > /dev/null
  ninja $command
  popd > /dev/null
  builder_finish_action success $command:$target
}

# ----------------------------------------------------------------------------
# utility functions
# ----------------------------------------------------------------------------

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
  if [[ -z ${EMSCRIPTEN_BASE+x} ]]; then
    if [[ -z ${EMCC+x} ]]; then
      local EMCC=`which emcc`
      [[ -z $EMCC ]] && builder_die "locate_emscripten: Could not locate emscripten (emcc) on the path or with \$EMCC or \$EMSCRIPTEN_BASE"
    fi
    [[ -x $EMCC ]] || builder_die "locate_emscripten: Variable EMCC ($EMCC) does not point to a valid executable emcc"
    EMSCRIPTEN_BASE="$(dirname "$EMCC")"
  fi

  [[ -x ${EMSCRIPTEN_BASE}/emcc ]] || builder_die "locate_emscripten: Variable EMSCRIPTEN_BASE ($EMSCRIPTEN_BASE) does not point to emcc's folder"
}

build_meson_cross_file_for_wasm() {
  if [ $BUILDER_OS == win ]; then
    local R=$(cygpath -w $(echo $EMSCRIPTEN_BASE) | sed 's_\\_\\\\_g')
  else
    local R=$(echo $EMSCRIPTEN_BASE | sed 's_/_\\/_g')
  fi
  sed -e "s/\$EMSCRIPTEN_BASE/$R/g" wasm.build.$BUILDER_OS.in > wasm.build
}