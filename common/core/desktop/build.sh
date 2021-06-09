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
  echo
  echo "Targets (all except install if not specified):"
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
  echo
  echo "Rust libraries will be in:  TARGETPATH/rust/<arch>/<buildtype>"
  echo "C++ libraries will be in:   TARGETPATH/<arch>/<buildtype>/src"
  echo "On Windows, <arch> will be 'x86' or 'x64'; elsewhere it is 'arch'"
  exit 0
}

get_builder_OS

CARGO_TARGET=--release
MESON_TARGET=release
HAS_TARGET=false
CONFIGURE=false
BUILD_RUST=false
BUILD_CPP=false
TESTS_RUST=false
TESTS_CPP=false
INSTALL_RUST=false
INSTALL_CPP=false
QUIET=false
TARGET_PATH="$THIS_DIR/build"
ADDITIONAL_ARGS=

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
    configure)
      HAS_TARGET=true
      CONFIGURE=true
      # meson depends on the rust build in order
      # to do its configure step, for now anyway
      BUILD_RUST=true
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
  CONFIGURE=true
  BUILD_RUST=true
  BUILD_CPP=true
  TESTS_RUST=true
  TESTS_CPP=true
fi

MESON_PATH="$TARGET_PATH/arch/$MESON_TARGET"

displayInfo "" \
    "VERSION: $VERSION" \
    "TIER: $TIER" \
    "CONFIGURE: $CONFIGURE" \
    "BUILD_RUST: $BUILD_RUST" \
    "BUILD_CPP: $BUILD_CPP" \
    "TESTS_RUST: $TESTS_RUST" \
    "TESTS_CPP: $TESTS_CPP" \
    "INSTALL_RUST: $INSTALL_RUST" \
    "INSTALL_CPP: $INSTALL_CPP" \
    "CARGO_TARGET: $CARGO_TARGET" \
    "MESON_TARGET: $MESON_TARGET" \
    "TARGET_PATH: $TARGET_PATH" \
    ""


build_test_rust() {
  local TARGETBASE="$1"
  if [ -z ${2+x} ]; then local TARGET=""; else local TARGET="$2"; fi

  if [ ! -z $TARGET ]; then
    local TARGET_FLAG=--target=$TARGET
  else
    local TARGET_FLAG=
  fi

  pushd "$THIS_DIR/src/rust" >/dev/null
  if $BUILD_RUST; then
    echo_heading "======= Building rust library for $TARGETBASE $TARGET ======="

    # Built library path for multi-arch (Windows) vs single (*nix)

    cargo build --target-dir="$TARGET_PATH/rust/$TARGETBASE" $TARGET_FLAG $CARGO_TARGET

    # On Windows, final output path is ./build/rust/<arch>/<arch_rust>/debug|release/<libraryname>
    # On other platforms, the final file is already in the right place (TARGET=="")
    if [ ! -z $TARGET ]; then
      local LIB="rust_mock_processor"

      # Library name on Windows vs *nix
      [ $os_id == "win" ] && \
        local LIBNAME=$LIB.lib || \
        local LIBNAME=lib$LIB.a

      local BUILT_PATH="$TARGET_PATH/rust/$TARGETBASE/$TARGET/$MESON_TARGET"
      local RUST_TARGET_PATH="$TARGET_PATH/rust/$TARGETBASE/$MESON_TARGET"
      cp "$BUILT_PATH/$LIBNAME" "$RUST_TARGET_PATH/$LIBNAME"
    fi
  fi

  if $TESTS_RUST; then
    echo_heading "======= Testing rust library for $TARGETBASE $TARGET ======="
    cargo test --target-dir="$TARGET_PATH/rust/$TARGETBASE" $TARGET $CARGO_TARGET
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

build_linux_macos() {

  # Build rust targets
  build_test_rust arch

  # Build meson targets
  if $CONFIGURE; then
    echo_heading "======= Configuring C++ library for $os_id ======="
    pushd $THIS_DIR > /dev/null
    meson $MESON_PATH --werror --buildtype $MESON_TARGET $ADDITIONAL_ARGS
    popd > /dev/null
  fi

  if $BUILD_CPP; then
    echo_heading "======= Building C++ library for $os_id ======="
    pushd $MESON_PATH > /dev/null
    ninja
    popd > /dev/null
  fi

  if $TESTS_CPP; then
    echo_heading "======= Testing C++ library for $os_id ======="
    pushd $MESON_PATH > /dev/null
    meson test --print-errorlogs
    popd > /dev/null
  fi

  if $INSTALL_RUST; then
    echo_heading "======= Installing Rust libraries for $os_id ======="
    # TODO
  fi

  if $INSTALL_CPP; then
    echo_heading "======= Installing C++ libraries for $os_id ======="
    pushd $MESON_PATH > /dev/null
    ninja install
    popd > /dev/null
  fi
}

build_macos() {
  build_linux_macos
}

build_linux() {
  build_linux_macos
}

###


case $os_id in
  "linux")
    build_linux
    ;;
  "mac")
    build_macos
    ;;
  "win")
    build_windows
    ;;
esac
