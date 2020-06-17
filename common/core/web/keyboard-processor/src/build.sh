#! /bin/bash
# 
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

display_usage ( ) {
    echo "build.sh [-skip-package-install | -S] [-test]"
    echo
    echo "  -skip-package-install  (or -S) skips the `lerna bootstrap` dependency check."
    echo "                         Intended for use when this script is called by another build script."
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
            set_default_vars
            FETCH_DEPS=false
            ;;
    esac
    shift # past argument
done

if [ $FETCH_DEPS = true ]; then
    verify_npm_setup
fi

# Compile web's `keyboard-processor` module.
npm run tsc -- -p src/tsconfig.json || fail "Failed to compile the core/web/keyboard-processor module."
