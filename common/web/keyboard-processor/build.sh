#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

# Ensures color var use in `builder_describe`'s argument respects the specified
# --color/--no-color option.
builder_check_color "$@"

builder_describe \
  "Compiles the web-oriented utility function module." \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action ${DESCRIBE_TERM_START}test${DESCRIBE_TERM_END} - emits CI-friendly test reports"

builder_parse "$@"

# START - Script parameter configuration
REPORT_STYLE=local  # Default setting.

if builder_has_option --ci; then
  REPORT_STYLE=ci

  echo "Replacing user-friendly test reports with CI-friendly versions."
fi

# END - Script parameter configuration

if builder_has_action configure; then
  verify_npm_setup

  "$KEYMAN_ROOT/common/web/keyman-version/build.sh" --report-scope

  builder_report success configure
fi

if builder_has_action clean; then
  npm run clean
  builder_report success clean
fi

if builder_has_action build; then
  npm run tsc -- --build "$THIS_SCRIPT_PATH/src/tsconfig.json"
  builder_report success build
fi

if builder_has_action test; then
  # Build test dependency
  pushd "$KEYMAN_ROOT/common/web/recorder"
  ./build.sh --report-scope
  popd

  npm run tsc -- --build "$THIS_SCRIPT_PATH/src/tsconfig.bundled.json"

  echo_heading "Running Keyboard Processor test suite"

  FLAGS=
  if [ $REPORT_STYLE == ci ]; then
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- --recursive $FLAGS ./tests/cases/

  builder_report success test
fi