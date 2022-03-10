#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE
THIS_DIR="$(dirname "$THIS_SCRIPT")"

display_usage() {
  echo "usage: build.sh [build options] [targets]"
  echo
  echo "Build options:"
  echo "  --clean, -c       Clean instead of build"
  echo "  --debug, -d       Debug build"
  echo "  --silent, -s      Suppress information messages"
  echo "  --keyboard, -k    Build only keyboards (not packages)"
  echo "  --kmcomp path     Specify path to kmcomp.exe, defaults"
  echo "                    to windows/bin/developer/kmcomp.exe"
  echo
  echo "Targets (all unless specified):"

  for d in "$THIS_DIR/"*/; do
    d="$(basename "$d")"
    echo "  $d"
  done

  echo
  exit 0
}

QUIET=false
DEBUG=false
CLEAN=false
KEYBOARDS_ONLY=false
CUSTOM_KMCOMP=
KMCOMP="$KEYMAN_ROOT/windows/bin/developer/kmcomp.exe"
TARGETS=()

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --help|-h|-\?)
      display_usage
      ;;
    --clean|-c)
      CLEAN=true
      ;;
    --debug|-d)
      DEBUG=true
      ;;
    --silent|-s)
      QUIET=true
      ;;
    --keyboard|-k)
      KEYBOARDS_ONLY=true
      ;;
    --kmcomp)
      shift
      KMCOMP="$1"
      CUSTOM_KMCOMP=true
      ;;
    *)
      TARGETS+=("$key")
  esac
  shift
done

# TODO: while this is intended to be cross platform, we
# don't currently have a binary version of kmcomp available
# during Linux and macOS builds, so that will need to be
# manually sourced.
KMCOMP_LAUNCHER=

if ! $CUSTOM_KMCOMP; then
  case "${OSTYPE}" in
    "cygwin")
      ;;
    "msys")
      ;;
    "darwin"*)
      # For Catalina (10.15) onwards, must use wine64
      base_macos_ver=10.15
      macos_ver=$(sw_vers -productVersion)
      if verlt "$macos_ver" "$base_macos_ver"; then
        KMCOMP_LAUNCHER=wine
      else
        # On Catalina, and later versions:
        # wine-4.12.1 works; wine-5.0, wine-5.7 do not.
        # retrieve these from:
        # `brew tap gcenx/wine && brew install --cask --no-quarantine wine-crossover`
        # may also need to `sudo spctl --master-disable`
        KMCOMP_LAUNCHER=wine64
        KMCOMP="$(dirname $KMCOMP)/kmcomp.x64.exe"
      fi
      ;;
    *)
      KMCOMP_LAUNCHER=wine
      ;;
  esac
fi

# Build list of available targets from subfolders, if none specified
if [ ${#TARGETS[@]} == 0 ]; then
  for d in "$THIS_DIR/"*/; do
    d="$(basename "$d")"
    TARGETS+=("$d")
  done
fi

if ! $QUIET; then
  displayInfo "" \
      "CLEAN: $CLEAN" \
      "DEBUG: $DEBUG" \
      "QUIET: $QUIET" \
      "KEYBOARDS_ONLY: $KEYBOARDS_ONLY" \
      "TARGETS: ${TARGETS[@]}" \
      ""
fi

clean() {
  local kpj="$1.kpj" ss= s=
  if $QUIET; then
    ss=-ss
    s=-s
  fi
  pushd "$1" > /dev/null
  if [ -f build.sh ]; then
    ./build.sh -c $s
  else
    $KMCOMP_LAUNCHER "$KMCOMP" -c $ss "$kpj"
  fi
  popd > /dev/null
}

build() {
  local kpj="$1.kpj" d= t= ss= target= s= k=
  if $KEYBOARDS_ONLY; then
    k=-k
    t=-t
    target="$1.kmn"
  fi
  if $DEBUG; then
    d=-d
  fi
  if $QUIET; then
    s=-s
    ss=-ss
  fi
  # -w - treat warnings as errors, we'll force this
  # -cfc - check filename conventions
  pushd "$1" > /dev/null
  if [ -f build.sh ]; then
    ./build.sh $d $k $s
  else
    $KMCOMP_LAUNCHER "$KMCOMP" $d $ss -w -cfc "$kpj" $t "$target"
  fi
  popd > /dev/null
}

###

for TARGET in "${TARGETS[@]}"; do
  if $CLEAN; then
    if ! $QUIET; then
      echo
      echo_heading "Cleaning target $TARGET"
      echo
    fi
    clean "$TARGET"
  else
    if ! $QUIET; then
      echo
      echo_heading "Building target $TARGET"
      echo
    fi
    build "$TARGET"
  fi
done

exit 0
