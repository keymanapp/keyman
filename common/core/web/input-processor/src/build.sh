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
    echo "  -test                  to compile for testing without re-fetching external dependencies"
    echo "                         or recompiling the lm-layer module."
    echo "  -skip-package-install  (or -S) skips the `lerna bootstrap` dependency check."
    echo ""
    echo "  If more than one target is specified, the last one will take precedence."
    exit 1
}

# Establish default build parameters
set_default_vars ( ) {
    BUILD_LMLAYER=true
    BUILD_CORE=true
    FETCH_DEPS=true
}

set_default_vars

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -test)
            set_default_vars
            BUILD_LMLAYER=false
            FETCH_DEPS=false
            ;;
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

if [ $BUILD_LMLAYER = true ]; then
    FLAGS="-skip-package-install"

    # Ensure that the LMLayer compiles properly, readying the build product for comsumption by KMW.
    cd ../../../../predictive-text/
    echo ""
    echo "Compiling the Language Modeling layer module..."
    ./build.sh $FLAGS || fail "Failed to compile the language modeling layer module."
    cd ../core/web/input-processor/src
    # TODO:  Move this back to KMW's main build script.  Consider it part of the dependency update.
    echo "Language Modeling layer compilation successful."
    echo ""
fi

if [ $BUILD_CORE = true ]; then
    FLAGS="-skip-package-install"

    # Ensure that the KeyboardProcessor module compiles properly.
    cd ../../keyboard-processor/src
    echo ""
    echo "Compiling Keyboard Processor module..."
    ./build.sh $FLAGS || fail "Dependency build failed; aborting"
    cd ../../input-processor/src
    echo "Keyboard Processor module compilation successful."
    echo ""
fi

# Compile web's `keyboard-processor` module.
echo "Compiling Input Processor module..."
npm run tsc -- -p src/tsconfig.json || fail "Failed to compile the core/web/input-processor module."
echo "Input Processor module compilation successful."
echo ""