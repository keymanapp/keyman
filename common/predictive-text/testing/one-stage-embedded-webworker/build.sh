#! /bin/bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#

display_usage ( ) {
  echo "build.sh [-clean]"
  echo
  echo "  -clean              to erase pre-existing build products before a re-build"
}

# Fails the build if a specified file does not exist.
assert ( ) {
  if ! [ -f $1 ]; then
    fail "Build failed."
    exit 1
  fi
}

# Prints a nice, common error message.
fail ( ) {
  FAILURE_MSG="$1"
  if [[ "$FAILURE_MSG" == "" ]]; then
    FAILURE_MSG="Unknown failure."
  fi
  echo "${ERROR_RED}$FAILURE_MSG${NORMAL}"
  exit 1
}

SOURCE="testing/one-stage-embedded-webworker"

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

npm run tsc -- -p $SOURCE/tsconfig.json

if [ $? -ne 0 ]; then
  fail "Compilation failed."
fi

echo "Typescript compilation successful."