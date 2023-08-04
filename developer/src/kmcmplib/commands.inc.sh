# shellcheck shell=bash
# no hashbang for .inc.sh

. "$KEYMAN_ROOT/resources/locate_emscripten.inc.sh"

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

build_meson_cross_file_for_wasm() {
  if [ $BUILDER_OS == win ]; then
    local R=$(cygpath -w $(echo $EMSCRIPTEN_BASE) | sed 's_\\_\\\\_g')
  else
    local R=$(echo $EMSCRIPTEN_BASE | sed 's_/_\\/_g')
  fi
  sed -e "s/\$EMSCRIPTEN_BASE/$R/g" wasm.build.$BUILDER_OS.in > wasm.build
}