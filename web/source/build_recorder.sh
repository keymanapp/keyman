#! /bin/bash
# 
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
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
COMPILED_FILE="recorder_InputEvents.js"
OUTPUT="../release/recorder"
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

$compilecmd -p $NODE_SOURCE/tsconfig.recorder.json
if [ $? -ne 0 ]; then
    fail "Typescript compilation failed."
fi

cp $OUTPUT/$COMPILED_FILE $ENGINE_TEST_OUTPUT
cp $OUTPUT/$COMPILED_FILE.map $ENGINE_TEST_OUTPUT