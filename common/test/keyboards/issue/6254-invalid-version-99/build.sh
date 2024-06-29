#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/build-utils.sh"
# . "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE
THIS_DIR="$(dirname "$THIS_SCRIPT")"

display_usage() {
  echo "Usage: build.sh [-c]"
  exit $1
}

# Parse args
shopt -s nocasematch

CLEAN=false
DEBUG=false
QUIET=false
KEYBOARDS_ONLY=false

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --help|-h|-\?)
      display_usage 0
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
    *)
      display_usage 1
      ;;
  esac
  shift
done

cd "$THIS_DIR"

if $CLEAN; then
  if ! $QUIET; then
    echo Cleaning the_99.kmp from build/
  fi
  rm -rf "$THIS_DIR"/build
else
  if ! $QUIET; then
    echo Copying the_99.kmp to build/
  fi
  mkdir -p "$THIS_DIR"/build
  cp "$THIS_DIR"/the_99.kmp build/the_99.kmp
fi