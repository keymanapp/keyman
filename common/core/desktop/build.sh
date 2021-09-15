#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
# NOTE: this is slightly non-standard; see longer discussion below
## END STANDARD BUILD SCRIPT INCLUDE

# This script does not use our normal shared build-utils.sh because Linux package builds
# cannot access anything outside of the `common/core/desktop` directory. This means that:
# 1. `shellHelperFunctions.sh`, `VERSION.md` and `TIER.md` are copied here by the script
#    `linux/scripts/dist.sh` for inclusion locally in Linux package builds.
# 2. `getversion.sh` and `gettier.sh` will use current folder if we can't access the
#    root level `VERSION.md` and `TIER.md`.
# 3. `$SCRIPTS_DIR` is set to this folder by the package build Makefile
#    `common/core/desktop/debian/rules`
SCRIPTS_DIR=${SCRIPTS_DIR:-$(dirname "$THIS_SCRIPT")/../../../resources}
. "${SCRIPTS_DIR}/shellHelperFunctions.sh"

THIS_DIR="$(dirname "$THIS_SCRIPT")"

pushd $THIS_DIR > /dev/null
VERSION=$(./getversion.sh)
TIER=$(./gettier.sh)
popd > /dev/null

display_usage() {
  echo "usage: build.sh [build options] [targets] [-- options to pass to c++ configure]"
  echo
  echo "Build options:"
  echo "  --debug, -d       Debug build"
  echo "  --target, -t      Target path (linux,macos only, default build/)"
  echo "  --platform, -p    Platform to build (wasm or native, default native)"
  echo
  echo "Targets (all except install if not specified):"
  echo "  clean             Clean target path"
  echo "  configure         Configure libraries (linux,macos only)"
  echo "  build             Build all libraries"
  echo "    build-rust        Build rust libraries"
  echo "    build-cpp         Build c++ libraries"
  echo "  tests             Run all tests"
  echo "    tests-rust        Run rust tests"
  echo "    tests-cpp         Run c++ and c++/rust integration tests"
  echo "  install           Install all libraries"
  echo "    install-rust      Install rust libraries"
  echo "    install-cpp       Install c++ libraries"
  echo "  uninstall         Uninstall all libraries"
  echo "    uninstall-rust    Uninstall rust libraries"
  echo "    uninstall-cpp     Uninstall c++ libraries"
  echo
  echo "Rust libraries will be in:      TARGETPATH/rust/<arch>/<buildtype>"
  echo "Rust web libraries will be in:  TARGETPATH/rust/web/<buildtype>"
  echo "C++ libraries will be in:       TARGETPATH/<arch>/<buildtype>/src"
  echo "WASM libraries will be in:      TARGETPATH/wasm/<buildtype>/src"
  echo "On Windows, <arch> will be 'x86' or 'x64'; elsewhere it is 'arch'"
  exit 0
}

get_builder_OS

CARGO_TARGET=--release
MESON_TARGET=release
HAS_TARGET=false
CLEAN=false
CONFIGURE=false
BUILD_RUST=false
BUILD_CPP=false
TESTS_RUST=false
TESTS_CPP=false
INSTALL_RUST=false
INSTALL_CPP=false
UNINSTALL_RUST=false
UNINSTALL_CPP=false
QUIET=false
TARGET_PATH="$THIS_DIR/build"
ADDITIONAL_ARGS=
PLATFORM=native

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --debug|-d)
      CARGO_TARGET=
      MESON_TARGET=debug
      ;;
    --help|-\?)
      display_usage
      ;;
    --target|-t)
      TARGET_PATH=$(readlink -f "$2")
      shift
      ;;
    --platform|-p)
      PLATFORM="$2"
      shift
      ;;
    configure)
      HAS_TARGET=true
      CONFIGURE=true
      # meson depends on the rust build in order
      # to do its configure step, for now anyway
      BUILD_RUST=true
      ;;
    clean)
      HAS_TARGET=true
      CLEAN=true
      ;;
    build)
      HAS_TARGET=true
      BUILD_RUST=true
      BUILD_CPP=true
      ;;
    build-rust)
      HAS_TARGET=true
      BUILD_RUST=true
      ;;
    build-cpp)
      HAS_TARGET=true
      BUILD_CPP=true
      ;;
    tests)
      HAS_TARGET=true
      TESTS_RUST=true
      TESTS_CPP=true
      ;;
    tests-rust)
      HAS_TARGET=true
      TESTS_RUST=true
      ;;
    tests-cpp)
      HAS_TARGET=true
      TESTS_CPP=true
      ;;
    install)
      HAS_TARGET=true
      INSTALL_RUST=true
      INSTALL_CPP=true
      ;;
    install-rust)
      HAS_TARGET=true
      INSTALL_RUST=true
      ;;
    install-cpp)
      HAS_TARGET=true
      INSTALL_CPP=true
      ;;
    uninstall)
      HAS_TARGET=true
      UNINSTALL_RUST=true
      # ninja records the files it installs, so unless we install first we don't know
      # what to uninstall. Installing will overwrite the existing files, if we then
      # then uninstall the files get removed - unless previously we had additional files.
      INSTALL_CPP=true
      UNINSTALL_CPP=true
      ;;
    uninstall-rust)
      HAS_TARGET=true
      UNINSTALL_RUST=true
      ;;
    uninstall-cpp)
      HAS_TARGET=true
      INSTALL_CPP=true
      UNINSTALL_CPP=true
      ;;
    --)
      shift
      ADDITIONAL_ARGS=$@
      break
      ;;
    *)
      fail "Invalid parameters. Use --help for help"
  esac
  shift
done

if ! $HAS_TARGET; then
  if [ ! -f "$TARGET_PATH" ]; then
    CONFIGURE=true
  fi
  BUILD_RUST=true
  BUILD_CPP=true
  TESTS_RUST=true
  TESTS_CPP=true
fi

if [[ $PLATFORM == wasm ]]; then
  MESON_PATH="$TARGET_PATH/wasm/$MESON_TARGET"
else
  MESON_PATH="$TARGET_PATH/arch/$MESON_TARGET"
fi

displayInfo "" \
    "VERSION: $VERSION" \
    "TIER: $TIER" \
    "PLATFORM: $PLATFORM" \
    "CONFIGURE: $CONFIGURE" \
    "CLEAN: $CLEAN" \
    "BUILD_RUST: $BUILD_RUST" \
    "BUILD_CPP: $BUILD_CPP" \
    "TESTS_RUST: $TESTS_RUST" \
    "TESTS_CPP: $TESTS_CPP" \
    "INSTALL_RUST: $INSTALL_RUST" \
    "INSTALL_CPP: $INSTALL_CPP" \
    "UNINSTALL_RUST: $UNINSTALL_RUST" \
    "UNINSTALL_CPP: $UNINSTALL_CPP" \
    "CARGO_TARGET: $CARGO_TARGET" \
    "MESON_TARGET: $MESON_TARGET" \
    "TARGET_PATH: $TARGET_PATH" \
    ""

clean() {
  rm -rf "$TARGET_PATH/"
}

path_remove() {
  # Delete path by parts so we can never accidentally remove sub paths
  PATH=${PATH//":$1:"/":"} # delete any instances in the middle
  PATH=${PATH/#"$1:"/} # delete any instance at the beginning
  PATH=${PATH/%":$1"/} # delete any instance in the at the end
}

run_cargo() {
  if [ $os_id == "win" ]; then
    # Remove /usr/bin/link from path because it confuses cargo
    local PATH_BACKUP="$PATH"
    path_remove /usr/bin
    path_remove /bin
  fi
  cargo "$@"
  if [ $os_id == "win" ]; then
    PATH="$PATH_BACKUP"
  fi
}

build_test_rust() {
  local TARGETBASE="$1"
  if [ -z ${2+x} ]; then local TARGET=""; else local TARGET="$2"; fi

  if [ ! -z $TARGET ]; then
    local TARGET_FLAG=--target=$TARGET
  else
    local TARGET_FLAG=
  fi

  # Cargo struggles with paths if Windows SDK environment does not
  # match; this overrides the library paths for this build
  if [ $os_id == "win" ]; then
    if [ ! -z ${LIB+x} ]; then
      LIB=${LIB//\\x86/\\$TARGETBASE}
      LIB=${LIB//\\x64/\\$TARGETBASE}
    fi
    if [ ! -z ${LIBPATH+x} ]; then
      LIBPATH=${LIBPATH//\\x86/\\$TARGETBASE}
      LIBPATH=${LIBPATH//\\x64/\\$TARGETBASE}
    fi
  fi

  pushd "$THIS_DIR/src/rust/$PLATFORM" >/dev/null
  if $BUILD_RUST; then
    echo_heading "======= Building rust library for $TARGETBASE, $TARGET ======="

    # Built library path for multi-arch (Windows) vs single (*nix)

    run_cargo build --target-dir="$TARGET_PATH/rust/$TARGETBASE" $TARGET_FLAG $CARGO_TARGET

    # On Windows, final output path is ./build/rust/<arch>/<arch_rust>/debug|release/<libraryname>
    # WASM is similar: ./build/rust/wasm/wasm_unknown_unknown/debug|release/<libraryname>
    # On Linux, macOS, the final file is already in the right place (TARGET=="")
    if [ ! -z $TARGET ]; then
      local LIB_MAP="keyman_keyboard_processor_$PLATFORM"

      # Library name on Windows vs *nix / WASM pref
      [[ $os_id == "win" && $TARGETBASE != "wasm" ]] && \
        local LIBNAME=$LIB_MAP.lib || \
        local LIBNAME=lib$LIB_MAP.a

      local BUILT_PATH="$TARGET_PATH/rust/$TARGETBASE/$TARGET/$MESON_TARGET"
      local RUST_TARGET_PATH="$TARGET_PATH/rust/$TARGETBASE/$MESON_TARGET"
      cp "$BUILT_PATH/$LIBNAME" "$RUST_TARGET_PATH/$LIBNAME"
    fi
  fi

  if $TESTS_RUST; then
    echo_heading "======= Testing rust library for $TARGETBASE $TARGET ======="
    run_cargo test --target-dir="$TARGET_PATH/rust/$TARGETBASE" $TARGET_FLAG $CARGO_TARGET
  fi
  popd >/dev/null
}

build_windows() {
  # Build targets for Windows

  # Build the rust targets, both x86 and x64
  build_test_rust x86 i686-pc-windows-msvc
  build_test_rust x64 x86_64-pc-windows-msvc

  # Build the meson targets, both x86 and x64 also
  # We need to use a batch file here so we can get
  # the Visual Studio build environment with vcvarsall.bat
  # TODO: if PATH is the only variable required, let's try and
  #       eliminate this difference in the build process

  if $BUILD_CPP; then
    if $TESTS_CPP; then
      echo_heading "======= Building and Testing C++ library for Windows (x86, x64) ======="
      cmd //C build.bat all $MESON_TARGET build tests
    else
      echo_heading "======= Building C++ library for Windows (x86, x64) ======="
      cmd //C build.bat all $MESON_TARGET build
    fi
  elif $TESTS_CPP; then
    echo_heading "======= Testing C++ library for Windows (x86, x64) ======="
    cmd //C build.bat all $MESON_TARGET tests
  fi
}

build_standard() {
  local BUILD_PLATFORM="$1"
  local ARCH="$2"
  local RUSTARCH=${3:-}
  if [ $# -gt 3 ]; then
    shift 3
    local STANDARD_MESON_ARGS="$*"
  else
    local STANDARD_MESON_ARGS=
  fi

  # Build rust targets
  build_test_rust "$ARCH" "$RUSTARCH"

  # Build meson targets
  if $CONFIGURE; then
    echo_heading "======= Configuring C++ library for $BUILD_PLATFORM ======="
    pushd "$THIS_DIR" > /dev/null
    meson setup "$MESON_PATH" --werror --buildtype $MESON_TARGET $STANDARD_MESON_ARGS $ADDITIONAL_ARGS
    popd > /dev/null
  fi

  if $BUILD_CPP; then
    echo_heading "======= Building C++ library for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    ninja
    popd > /dev/null
  fi

  if $TESTS_CPP; then
    echo_heading "======= Testing C++ library for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    meson test --print-errorlogs
    popd > /dev/null
  fi

  if $INSTALL_RUST; then
    echo_heading "======= Installing Rust libraries for $BUILD_PLATFORM ======="
    # TODO
  fi

  if $INSTALL_CPP; then
    echo_heading "======= Installing C++ libraries for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    ninja install
    popd > /dev/null
  fi

  if $UNINSTALL_RUST; then
    echo_heading "======= Uninstalling Rust libraries for $BUILD_PLATFORM ======="
    # TODO
  fi

  if $UNINSTALL_CPP; then
    echo_heading "======= Uninstalling C++ libraries for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    ninja uninstall
    popd > /dev/null
  fi
}

locate_emscripten() {
  local EMCC=`which emcc`
  [ -z "$EMCC" ] && fail "Could not locate emscripten (emcc)"
  EMSCRIPTEN_BASE="$(dirname "$EMCC")"
}

build_meson_cross_file_for_wasm() {
  if [ $os_id == win ]; then
    local R=$(cygpath -w $(echo $EMSCRIPTEN_BASE) | sed 's_\\_\\\\_g')
  else
    local R=$(echo $EMSCRIPTEN_BASE | sed 's_/_\\/_g')
  fi
  sed -e "s/\$EMSCRIPTEN_BASE/$R/g" wasm.build.$os_id.in > wasm.build
}

###

if $CLEAN; then
  clean
fi

if [[ $PLATFORM == native ]]; then
  case $os_id in
    "linux")
      build_standard $os_id arch
      ;;
    "mac")
      build_standard $os_id arch
      ;;
    "win")
      build_windows
      ;;
  esac
else
  locate_emscripten
  build_meson_cross_file_for_wasm
  build_standard wasm wasm wasm32-unknown-unknown --cross-file wasm.defs.build --cross-file wasm.build --default-library static
fi
