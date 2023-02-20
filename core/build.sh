#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

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
  echo "    build-cpp         Build c++ libraries"
  echo "  tests             Run all tests"
  echo "    tests-cpp         Run c++ tests"
  echo "  install           Install all libraries"
  echo "    install-cpp       Install c++ libraries"
  echo "  uninstall         Uninstall all libraries"
  echo "    uninstall-cpp     Uninstall c++ libraries"
  echo
  echo "C++ libraries will be in:       TARGETPATH/<arch>/<buildtype>/src"
  echo "WASM libraries will be in:      TARGETPATH/wasm/<buildtype>/src"
  echo "On Windows, <arch> will be 'x86' or 'x64'; elsewhere it is 'arch'"
  exit 0
}

MESON_TARGET=release
HAS_TARGET=false
CLEAN=false
CONFIGURE=false
BUILD_CPP=false
TESTS_CPP=false
INSTALL_CPP=false
UNINSTALL_CPP=false
QUIET=false
TARGET_PATH="$THIS_SCRIPT_PATH/build"
ADDITIONAL_ARGS=
PLATFORM=native

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --debug|-d)
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
      ;;
    clean)
      HAS_TARGET=true
      CLEAN=true
      ;;
    build)
      HAS_TARGET=true
      BUILD_CPP=true
      ;;
    *-rust)
      echo "$key: Rust was removed in <https://github.com/keymanapp/keyman/issues/6290>"
      ;;
    build-cpp)
      HAS_TARGET=true
      BUILD_CPP=true
      ;;
    tests)
      HAS_TARGET=true
      TESTS_CPP=true
      ;;
    tests-cpp)
      HAS_TARGET=true
      TESTS_CPP=true
      ;;
    install)
      HAS_TARGET=true
      INSTALL_CPP=true
      ;;
    install-cpp)
      HAS_TARGET=true
      INSTALL_CPP=true
      ;;
    uninstall)
      HAS_TARGET=true
      # ninja records the files it installs, so unless we install first we don't know
      # what to uninstall. Installing will overwrite the existing files, if we then
      # then uninstall the files get removed - unless previously we had additional files.
      INSTALL_CPP=true
      UNINSTALL_CPP=true
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
      builder_die "Invalid parameters. Use --help for help"
  esac
  shift
done

if ! $HAS_TARGET; then
  if [ ! -f "$TARGET_PATH" ]; then
    CONFIGURE=true
  fi
  BUILD_CPP=true
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
    "BUILD_CPP: $BUILD_CPP" \
    "TESTS_CPP: $TESTS_CPP" \
    "INSTALL_CPP: $INSTALL_CPP" \
    "UNINSTALL_CPP: $UNINSTALL_CPP" \
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

build_windows() {
  # Build targets for Windows

  # Build the meson targets, both x86 and x64 also
  # We need to use a batch file here so we can get
  # the Visual Studio build environment with vcvarsall.bat
  # TODO: if PATH is the only variable required, let's try and
  #       eliminate this difference in the build process

  if $BUILD_CPP; then
    if $TESTS_CPP; then
      builder_heading "======= Building and Testing C++ library for Windows (x86, x64) ======="
      cmd //C build.bat all $MESON_TARGET build tests
    else
      builder_heading "======= Building C++ library for Windows (x86, x64) ======="
      cmd //C build.bat all $MESON_TARGET build
    fi
  elif $TESTS_CPP; then
    builder_heading "======= Testing C++ library for Windows (x86, x64) ======="
    cmd //C build.bat all $MESON_TARGET tests
  fi
}

build_standard() {
  local BUILD_PLATFORM="$1"
  local ARCH="$2"
  local RUSTARCH=${3:-}
  # RUSTARCH is not currently used.
  if [ $# -gt 3 ]; then
    shift 3
    local STANDARD_MESON_ARGS="$*"
  else
    local STANDARD_MESON_ARGS=
  fi

  # Build meson targets
  if $CONFIGURE; then
    builder_heading "======= Configuring C++ library for $BUILD_PLATFORM ======="
    pushd "$THIS_SCRIPT_PATH" > /dev/null
    meson setup "$MESON_PATH" --werror --buildtype $MESON_TARGET $STANDARD_MESON_ARGS $ADDITIONAL_ARGS
    popd > /dev/null
  fi

  if $BUILD_CPP; then
    builder_heading "======= Building C++ library for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    ninja
    popd > /dev/null
  fi

  if $TESTS_CPP; then
    builder_heading "======= Testing C++ library for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    meson test --print-errorlogs
    popd > /dev/null
  fi

  if $INSTALL_CPP; then
    builder_heading "======= Installing C++ libraries for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    ninja install
    popd > /dev/null
  fi

  if $UNINSTALL_CPP; then
    builder_heading "======= Uninstalling C++ libraries for $BUILD_PLATFORM ======="
    pushd "$MESON_PATH" > /dev/null
    ninja uninstall
    popd > /dev/null
  fi
}

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
  if [[ $BUILDER_OS == win ]]; then
    local R=$(cygpath -w $(echo $EMSCRIPTEN_BASE) | sed 's_\\_\\\\_g')
  else
    local R=$(echo $EMSCRIPTEN_BASE | sed 's_/_\\/_g')
  fi
  sed -e "s/\$EMSCRIPTEN_BASE/$R/g" wasm.build.$BUILDER_OS.in > wasm.build
}

###

if $CLEAN; then
  clean
fi

if [[ $PLATFORM == native ]]; then
  case $BUILDER_OS in
    linux)
      build_standard linux arch
      ;;
    mac)
      build_standard mac arch
      ;;
    win)
      build_windows
      ;;
  esac
else
  locate_emscripten
  build_meson_cross_file_for_wasm
  build_standard wasm wasm wasm32-unknown-unknown --cross-file wasm.defs.build --cross-file wasm.build --default-library static
fi
