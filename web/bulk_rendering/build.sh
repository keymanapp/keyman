#! /bin/bash
#
# Compile the KeymanWeb bulk-renderer module for use with developing/running engine tests.

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

set_keyman_standard_build_path

display_usage ( ) {
  echo "Usage: build.sh [flags...] [commands...]"
  echo
  echo "Commands:"
  echo "  configure              runs 'npm ci' on root folder"
  echo "  build                  (default) builds bulk_renderer to ../release/renderer"
  echo
  echo "Flags:"
  echo "  -v, --verbose          Use verbose logging"
  echo "  -h, --help             Print this help"
}

################################ Main script ################################

builder_init "configure build" "$@"

if builder_has_action configure; then
  verify_npm_setup
  builder_report configure success
fi

if builder_has_action build; then
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.json" $builder_verbose
  builder_report build success
fi
