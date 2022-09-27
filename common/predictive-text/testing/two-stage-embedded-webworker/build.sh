#!/usr/bin/env bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../resources/build/build-utils.sh"
. "$REPO_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage ( ) {
  echo "build.sh [-clean]"
  echo
  echo "  -clean              to erase pre-existing build products before a re-build"
}

SOURCE="testing/two-stage-embedded-webworker"

COMPILED_WORKER="worker.js"
EMBEDDED_WORKER="embedded_worker.js"

echo "Node.js + dependencies check"
npm install --no-optional

if [ $? -ne 0 ]; then
  fail "Build environment setup error detected!  Please ensure Node.js is installed!"
fi

# A nice, extensible method for -clean operations.  Add to this as necessary.
clean ( ) {
  rm -rf "./*.js"
  if [ $? -ne 0 ]; then
    fail "Failed to erase the prior build."
  fi
}

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -clean)
      clean
      ;;
  esac
  shift # past the processed argument
done

npm run tsc -- -p $SOURCE/worker/tsconfig.json
if [ $? -ne 0 ]; then
  fail "Worker compilation failed."
fi

rm $EMBEDDED_WORKER &> /dev/null

echo "var LMLayerWorker = function() {" >> $EMBEDDED_WORKER
cat $COMPILED_WORKER >> $EMBEDDED_WORKER
echo "" >> $EMBEDDED_WORKER
echo "}" >> $EMBEDDED_WORKER

npm run tsc -- -p $SOURCE/tsconfig.json
if [ $? -ne 0 ]; then
  fail "Final compilation failed."
fi

echo "Typescript compilation successful."