#!/bin/bash
# 
# Compiles common TS-based utility functions for use among Keyman's codebase

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(dirname "$THIS_SCRIPT")"
KEYMAN_ROOT="$(dirname "$THIS_SCRIPT")/../../../.."
. "$KEYMAN_ROOT/resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage ( ) {
    echo "build.sh [-skip-package-install]"
    echo
    echo "  -skip-package-install  skips the \`lerna bootstrap\` dependency check."
    echo "                            (or -S) Intended for use when this script is called by another build script."
    echo ""
    echo "  If more than one target is specified, the last one will take precedence."
    exit 1
}

# Establish default build parameters
set_default_vars ( ) {
    FETCH_DEPS=true
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

# Definition of global compile constants
OUTPUT_DIR="dist"
OUTPUT="index.js"

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

$compilecmd -p "$SCRIPT_DIR/tsconfig.json"
if [ $? -ne 0 ]; then
    fail "Utility-function package compilation failed."
fi
