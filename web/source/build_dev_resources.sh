#! /bin/bash
# 
# Compiles development-related KeymanWeb resources for use with developing/running tests.
#   - the Recorder module (for engine tests)
#   - the DOM module (for touch-alias and element-interface tests)
#

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
echo "Node.js + dependencies check"
npm install --no-optional

if [ $? -ne 0 ]; then
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"
fi

# Definition of global compile constants
SCRIPT_TAGS=( "recorder"                "dev_resources" )
PRODUCTS=(    "recorder_InputEvents.js" "dev_resources.js")
FOLDERS=(     "recorder"                "dev_resources")
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