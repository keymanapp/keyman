#! /bin/bash
# 
# Compile the KeymanWeb bulk-renderer module for use with developing/running engine tests.

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Fails the build if a specified file does not exist.
assert ( ) {
    if ! [ -f $1 ]; then
        fail "Build failed."
        exit 1
    fi
}

fail() {
    FAILURE_MSG="$1"
    if [[ "$FAILURE_MSG" == "" ]]; then
        FAILURE_MSG="Unknown failure"
    fi
    echo "${ERROR_RED}$FAILURE_MSG${NORMAL}"
    exit 1
}

# Ensure the dependencies are downloaded.  --no-optional should help block fsevents warnings.
verify_npm_setup

# Definition of global compile constants
COMPILED_FILE="bulk_render.js"
OUTPUT="../release/renderer"
NODE_SOURCE="bulk_rendering"
#ENGINE_TEST_OUTPUT="../unit_tests/"

readonly OUTPUT
readonly NODE_SOURCE
#readonly ENGINE_TEST_OUTPUT

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

$compilecmd -p $NODE_SOURCE/tsconfig.json
if [ $? -ne 0 ]; then
    fail "Typescript compilation failed."
fi

#cp $OUTPUT/$COMPILED_FILE $ENGINE_TEST_OUTPUT
#cp $OUTPUT/$COMPILED_FILE.map $ENGINE_TEST_OUTPUT