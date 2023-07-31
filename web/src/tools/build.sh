#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's development & unit-testing tools" \
  "@/common/web/keyman-version" \
  "@/common/web/keyboard-processor" \
  "configure" \
  "clean" \
  "build" \
  "test" \
  "--ci         Does nothing for this script" \
  ":bulk_rendering=testing/bulk_rendering   Builds the bulk-rendering tool used to validate changes to OSK display code" \
  ":recorder=testing/recorder               Builds the KMW recorder tool used for development of unit-test resources" \
  ":sourcemap-root=building/sourcemap-root  Builds the sourcemap-cleaning tool used during minification of app/ builds"

builder_parse "$@"

### CLEAN ACTIONS

if builder_start_action clean:bulk_rendering; then
  testing/bulk_rendering/build.sh clean

  builder_finish_action success clean:bulk_rendering
fi

if builder_start_action clean:recorder; then
  testing/recorder/build.sh clean

  builder_finish_action success clean:recorder
fi

if builder_start_action clean:sourcemap-root; then
  building/sourcemap-root/build.sh clean

  builder_finish_action success clean:sourcemap-root
fi

### BUILD ACTIONS

if builder_start_action build:bulk_rendering; then
  testing/bulk_rendering/build.sh

  builder_finish_action success build:bulk_rendering
fi

if builder_start_action build:recorder; then
  testing/recorder/build.sh

  builder_finish_action success build:recorder
fi

if builder_start_action build:sourcemap-root; then
  building/sourcemap-root/build.sh

  builder_finish_action success build:sourcemap-root
fi