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

  # Removes ICU cached components so it will be re-downloaded and built. Note:
  # we could use `git clean`, but this clarifies exactly what is deleted, and
  # but we try not to use git commands in build scripts, to maintain clear
  # responsibility
  rm -rf \
    "$THIS_SCRIPT_PATH/subprojects/icu" \
    "$THIS_SCRIPT_PATH/subprojects/*.tgz" \
    "$THIS_SCRIPT_PATH/subprojects/*.zip"

  builder_finish_action success clean:$target
}

# ----------------------------------------------------------------------------
# configure
# ----------------------------------------------------------------------------

do_configure() {
  local target=$1
  builder_start_action configure:$target || return 0

  local STANDARD_MESON_ARGS="$MESON_OPTION_keyman_core_tests --default-library both"
  local MESON_CROSS_FILE=

  if [[ -f "$THIS_SCRIPT_PATH/cross-$target.build" ]]; then
    MESON_CROSS_FILE="--cross-file cross-$target.build"
  fi

  builder_heading "======= Configuring $target ======="

  if [[ $target == wasm ]]; then
    # do_configure_wasm
    locate_emscripten
    build_meson_cross_file_for_wasm
    STANDARD_MESON_ARGS="$STANDARD_MESON_ARGS --cross-file wasm.defs.build --cross-file wasm.build --default-library static"
  fi

  pushd "$THIS_SCRIPT_PATH" > /dev/null
  # Additional arguments are used by Linux build, e.g. -Dprefix=${INSTALLDIR}
  meson setup "$MESON_PATH" $MESON_CROSS_FILE --werror --buildtype $BUILDER_CONFIGURATION $STANDARD_MESON_ARGS "${builder_extra_params[@]}"
  popd > /dev/null

  builder_finish_action success configure:$target
}

# ----------------------------------------------------------------------------
# build
# ----------------------------------------------------------------------------

do_build() {
  local target=$1
  builder_start_action build:$target || return 0
  meson compile -C "$MESON_PATH"
  builder_finish_action success build:$target
}

# ----------------------------------------------------------------------------
# test
# ----------------------------------------------------------------------------

do_test() {
  local target=$1
  builder_start_action test:$target || return 0
  if [[ $target == wasm ]] && [[ $BUILDER_OS == mac ]]; then
    # 11794 -- parallel tests failing on some mac build agents; temporary
    # mitigation until we diagnose root cause
    meson test -j 1 -C "$MESON_PATH" $testparams
  else
    meson test -C "$MESON_PATH" $testparams
  fi
  builder_finish_action success test:$target
}

# ----------------------------------------------------------------------------
# install and uninstall
# ----------------------------------------------------------------------------

do_install() {
  local target=$1
  builder_start_action install:$target || return 0
  meson install -C "$MESON_PATH"
  builder_finish_action success install:$target
}

do_uninstall() {
  local target=$1
  builder_start_action uninstall:$target || return 0
  pushd "$MESON_PATH" > /dev/null
  # Note: there is no meson uninstall command, which means
  # this probably won't work on Windows
  ninja uninstall
  popd > /dev/null
  builder_finish_action success uninstall:$target
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
