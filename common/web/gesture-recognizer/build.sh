#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_describe "Builds the gesture-recognition model for Web-based on-screen keyboards" \
  "@/common/web/es-bundling build" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":module" \
  ":tools  tools for testing & developing test resources for this module" \
  "--ci    sets the --ci option for child scripts (i.e, the $(builder_term test) action)"

builder_describe_outputs \
  configure        /node_modules \
  build:module     /common/web/gesture-recognizer/build/lib/index.mjs \
  build:tools      /common/web/gesture-recognizer/build/tools/lib/index.mjs

builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

if builder_start_action build:module; then
  # Build
  tsc --build $builder_verbose
  node build-bundler.js
  builder_finish_action success build:module
fi

if builder_start_action build:tools; then
  src/tools/build.sh build
  builder_finish_action success build:tools
fi

if builder_start_action test:module; then
  if builder_has_option --ci; then
    ./test.sh --ci
  else
    ./test.sh
  fi
  builder_finish_action success test:module
fi

if builder_has_action test:tools && ! builder_has_action test:module; then
  echo "The $(builder_term test:tools) action is currently a no-op."
fi