#!/usr/bin/env bash
#
# Compiles development-related KeymanWeb resources for use with developing/running tests.
#   - the Recorder module (for engine tests)
#   - the DOM module (for touch-alias and element-interface tests)

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$REPO_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

verify_npm_setup

ENGINE_TEST_OUTPUT="../unit_tests/"
readonly ENGINE_TEST_OUTPUT

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../../node_modules/.bin:$PATH"

npm run tsc -- --build source/tsconfig.dev_resources.json
if [ $? -ne 0 ]; then
    fail "Typescript compilation failed for 'dev_resources'."
fi

cp ../release/dev_resources/dev_resources.js "$ENGINE_TEST_OUTPUT/dev_resources.js"
cp ../release/dev_resources/dev_resources.js.map "$ENGINE_TEST_OUTPUT/dev_resources.js.map"
