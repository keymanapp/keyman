#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/common/web/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe "Builds the gesture-recognition model for Web-based on-screen keyboards" \
  "@/common/web/es-bundling build" \
  "@/web/src/engine/common/utils build" \
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

function do_configure() {
  verify_npm_setup

  # Configure Web browser-engine testing environments.  As is, this should only
  # make changes when we update the dependency, even on our CI build agents.
  playwright install
}

builder_run_action configure do_configure

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

if builder_start_action build:module; then
  # Build
  tsc --build $builder_verbose

  $BUNDLE_CMD    "${KEYMAN_ROOT}/common/web/gesture-recognizer/build/obj/index.js" \
    --out        "${KEYMAN_ROOT}/common/web/gesture-recognizer/build/lib/index.mjs" \
    --format esm

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
