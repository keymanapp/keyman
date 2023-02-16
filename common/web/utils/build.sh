#!/usr/bin/env bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

# Ensures color var use in `builder_describe`'s argument respects the specified
# --color/--no-color option.
builder_check_color "$@"

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@../keyman-version" \
  configure clean build test \
  "--ci    For use with action ${BUILDER_TERM_START}test${BUILDER_TERM_END} - emits CI-friendly test reports"

builder_describe_outputs \
  configure "/node_modules" \
  build     "build/index.js"

builder_parse "$@"

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  npm run clean
  builder_finish_action success clean
fi

if builder_start_action build; then
  # Note: in a dependency build, we'll expect utils to be built by tsc -b
  if builder_is_dep_build; then
    echo "[$THIS_SCRIPT_IDENTIFIER] skipping tsc -b; will be completed by $builder_dep_parent"
  else
    npm run tsc -- --build "$THIS_SCRIPT_PATH/tsconfig.json"
    node build-bundler.js

    # So... tsc does declaration-bundling on its own pretty well, at least for local development.
    npm run tsc -- --emitDeclarationOnly --outFile ./build/lib/index.d.ts
  fi
  builder_finish_action success build
fi

if builder_start_action test; then
  echo_heading "Running web-utils test suite"

  FLAGS=
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- --recursive $FLAGS ./src/test/

  builder_finish_action success test
fi