#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

builder_describe "app test module" \
  @../library \
  clean \
  configure \
  build

builder_parse "$@"

builder_describe_outputs \
  configure:project out.configure \
  build:project     out.build

if builder_start_action clean:project; then
  rm -f out.configure out.build
  builder_finish_action success clean:project
fi

if builder_start_action configure:project; then
  echo " ... doing the 'configure' action for 'app'"
  touch out.configure
  builder_finish_action success configure:project
fi

if builder_start_action build:project; then
  echo " ... doing the 'build' action for 'app'"
  touch out.build
  builder_finish_action success build:project
fi
