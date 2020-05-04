#! /bin/bash
# 
# Compiles development-related KeymanWeb resources for use with developing/running tests.
#   - the Recorder module (for engine tests)
#   - the DOM module (for touch-alias and element-interface tests)

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

# Ensure the dependencies are downloaded.  --no-optional should help block fsevents warnings.
verify_npm_setup

# Definition of global compile constants
SCRIPT_TAGS=( "dev_resources" )
PRODUCTS=(    "dev_resources.js")
FOLDERS=(     "dev_resources")
OUTPUT_BASE="../release"
PRODUCT_COUNT=${#PRODUCTS[@]}

# COMPILED_FILE="recorder_InputEvents.js"
# SCRIPT_TAG="recorder"
# OUTPUT="../release/recorder"

NODE_SOURCE="source"
ENGINE_TEST_OUTPUT="../unit_tests/"

readonly OUTPUT
readonly NODE_SOURCE
readonly ENGINE_TEST_OUTPUT

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

for (( n=0; n<$PRODUCT_COUNT; n++ ))  # Apparently, args ends up zero-based, meaning $2 => n=1.
do
  $compilecmd -p $NODE_SOURCE/tsconfig.${SCRIPT_TAGS[$n]}.json
  if [ $? -ne 0 ]; then
      fail "Typescript compilation failed for '${SCRIPT_TAGS[$n]}'."
  fi

  PRODUCT=$OUTPUT_BASE/${FOLDERS[$n]}/${PRODUCTS[$n]}
  cp $PRODUCT $ENGINE_TEST_OUTPUT
  cp $PRODUCT.map $ENGINE_TEST_OUTPUT
# ###
#   target=${args[$n]}
#   cp -f $INTERMEDIATE/$target $dest/$target
#   cp -f $INTERMEDIATE/$target.map $dest/$target.map
done