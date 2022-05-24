#!/bin/bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

display_usage ( ) {
    echo "build.sh [-skip-package-install]"
    echo
    echo "  -skip-package-install  skips the \`npm install\` dependency check."
    echo "                            (or -S) Intended for use when this script is called by another build script."
    echo ""
    echo "  If more than one target is specified, the last one will take precedence."
    exit 1
}

# Establish default build parameters
set_default_vars ( ) {
    FETCH_DEPS=true
    # We need to build keyman-version and lm-worker with a script for now
    "$KEYMAN_ROOT/common/web/keyman-version/build.sh" || fail "Could not build keyman-version"
}

set_default_vars

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -skip-package-install|-S)
            FETCH_DEPS=false
            ;;
    esac
    shift # past argument
done

if [ "$FETCH_DEPS" = true ]; then
    verify_npm_setup
fi

npm run tsc -- --build "$THIS_SCRIPT_PATH/tsconfig.json"
if [ $? -ne 0 ]; then
    fail "Utility-function package compilation failed."
fi
