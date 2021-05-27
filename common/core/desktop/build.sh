#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$(dirname "$THIS_SCRIPT")/../../../resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage() {
  echo "usage: build.sh [build options] [targets]"
  echo
  echo "Build options:"
  echo "  --debug, -d       Debug build"
  echo
  echo "Targets (all if not specified):"
  echo "  build             Build all libraries"
  echo "    build-rust        Build rust libraries"
  echo "    build-cpp         Build c++ libraries"
  echo "  tests             Run all tests"
  echo "    tests-rust        Run rust tests"
  echo "    tests-cpp         Run c++ and c++/rust integration tests"
  echo
  echo "Rust libraries will be in:  build/rust/<arch>/<buildtype>"
  echo "C++ libraries will be in:   build/<arch>/<buildtype>/src"
  echo "On Windows, <arch> will be 'x86' or 'x64'; elsewhere it is 'arch'"
  exit 0
}

get_builder_OS

THIS_DIR="$(dirname "$THIS_SCRIPT")"
CARGO_TARGET=--release
MESON_TARGET=release
HAS_TARGET=false
BUILD_RUST=false
BUILD_CPP=false
TESTS_RUST=false
TESTS_CPP=false
QUIET=false

# Parse args
shopt -s nocasematch


while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --debug|-d)
      CARGO_TARGET=
      MESON_TARGET=debug
      ;;
    --help|-?)
        display_usage
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
    *)
      fail "Invalid parameters. Use --help for help"
  esac
  shift
done

if ! $HAS_TARGET; then
  BUILD_RUST=true
  BUILD_CPP=true
  TESTS_RUST=true
  TESTS_CPP=true
fi

    # "CLEAN: $CLEAN" \

displayInfo "" \
    "VERSION: $VERSION" \
    "TIER: $TIER" \
    "BUILD_RUST: $BUILD_RUST" \
    "BUILD_CPP: $BUILD_CPP" \
    "TESTS_RUST: $TESTS_RUST" \
    "TESTS_CPP: $TESTS_CPP" \
    "CARGO_TARGET: $CARGO_TARGET" \
    "MESON_TARGET: $MESON_TARGET" \
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

    cargo build --target-dir="$THIS_DIR/build/rust/$TARGETBASE" $TARGET_FLAG $CARGO_TARGET

    # Final output path is ./build/rust/<arch>/debug|release/<libraryname>
    if [ ! -z $TARGET ]; then
      local LIB="rust_mock_processor"

      # Library name on Windows vs *nix
      [ $os_id == "win" ] && \
        local LIBNAME=$LIB.lib || \
        local LIBNAME=lib$LIB.a

      local BUILT_PATH="$THIS_DIR/build/rust/$TARGETBASE/$TARGET/$MESON_TARGET"
      local TARGET_PATH="$THIS_DIR/build/rust/$TARGETBASE/$MESON_TARGET"
      cp "$BUILT_PATH/$LIBNAME" "$TARGET_PATH/$LIBNAME"
    fi
  fi

  if $TESTS_RUST; then
    echo_heading "======= Testing rust library for $TARGETBASE $TARGET ======="
    cargo test --target-dir="$THIS_DIR/build/rust/$TARGETBASE" $TARGET $CARGO_TARGET
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
  if $BUILD_CPP; then
    echo_heading "======= Building C++ library for $os_id ======="
    meson build/arch/$MESON_TARGET --werror --buildtype $MESON_TARGET
    cd build/arch/$MESON_TARGET
    ninja
    cd ../../..
  fi

  if $TESTS_CPP; then
    echo_heading "======= Testing C++ library for $os_id ======="
    cd build/arch/$MESON_TARGET
    meson test --print-errorlogs
    cd ../../..
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
