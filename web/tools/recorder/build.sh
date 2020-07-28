#! /bin/bash
# 
# Compiles development-related KeymanWeb resources for use with developing/running tests.
#   - the Recorder module (for engine tests)
#   - the DOM module (for touch-alias and element-interface tests)

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SCRIPT_DIR="$(dirname "$THIS_SCRIPT")"

# Ensure the dependencies are downloaded.  --no-optional should help block fsevents warnings.
verify_npm_setup

# Definition of global compile constants
OUTPUT_DIR="build"
OUTPUT="recorder_InputEvents.js"

ENGINE_TEST_OUTPUT="../../unit_tests/"

readonly ENGINE_TEST_OUTPUT

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../../node_modules/.bin:$PATH"

pushd "$KEYMAN_ROOT/common/core/web/tools/recorder/src"
./build.sh -skip-package-install || fail "recorder-core compilation failed."
popd

compiler="npm run tsc --"
compilecmd="$compiler"

$compilecmd -p "$SCRIPT_DIR/tsconfig.json"
if [ $? -ne 0 ]; then
    fail "KeymanWeb test sequence recorder compilation failed."
fi

PRODUCT="$SCRIPT_DIR/$OUTPUT_DIR/$OUTPUT"
cp $PRODUCT $ENGINE_TEST_OUTPUT
cp $PRODUCT.map $ENGINE_TEST_OUTPUT

