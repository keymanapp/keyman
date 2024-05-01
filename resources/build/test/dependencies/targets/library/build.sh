#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

builder_describe "library test module" \
  :first \
  :second \
  clean \
  configure \
  build

builder_parse "$@"

builder_describe_outputs \
  configure:first  out.first.configure \
  build:first      out.first.build \
  configure:second out.second.configure \
  build:second     out.second.build

if builder_start_action clean:first; then
  rm -f out.first.configure out.first.build
  builder_finish_action success clean:first
fi

if builder_start_action clean:second; then
  rm -f out.second.configure out.second.build
  builder_finish_action success clean:second
fi

if builder_start_action configure:first; then
  echo " ... doing the 'configure:first' action for 'library'"
  touch out.first.configure
  builder_finish_action success configure:first
fi

if builder_start_action configure:second; then
  echo " ... doing the 'configure:second' action for 'library'"
  touch out.second.configure
  builder_finish_action success configure:second
fi

if builder_start_action build:first; then
  echo " ... doing the 'build:first' action for 'library'"
  touch out.first.build
  builder_finish_action success build:first
fi

if builder_start_action build:second; then
  echo " ... doing the 'build:second' action for 'library'"
  touch out.second.build
  builder_finish_action success build:second
fi
