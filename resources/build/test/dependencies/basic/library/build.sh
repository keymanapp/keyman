#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

# Test builder_describe_outputs and dependencies

builder_describe "library test module" \
  clean \
  configure \
  build

builder_parse "$@"

builder_describe_outputs \
  configure:project out.configure \
  build:project     out.build

echo ...

if builder_start_action clean:project; then
  rm -f out.configure out.build
  builder_finish_action success clean:project
fi

if builder_start_action configure:project; then
  echo " ... doing the 'configure' action for 'library'"
  touch out.configure
  builder_finish_action success configure:project
fi

if builder_start_action build:project; then
  echo " ... doing the 'build' action for 'library'"
  touch out.build
  builder_finish_action success build:project
fi
