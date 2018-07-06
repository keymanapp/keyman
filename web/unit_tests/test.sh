#! /bin/bash

# A simple utility script to facilitate our different modes for unit-testing KMW.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
    echo "test.sh [-CI | -debug | -? | -h | -help] [-reporter <reporter>]"
    echo
    echo "  -CI               to run unit tests in CI mode on BrowserStack."
    echo "                    This script requires your credentials to be set in environment variables - see "
    echo "                    https://stackoverflow.com/questions/32450546/hiding-browserstack-key-in-karma"
    echo ""
    echo "  -debug            to establish a Karma server that facilitates unit test debugging"
    echo "                    Not compatible with -CI."
    echo ""
    echo "  -reporter         sets the test engine to utilize the specified <reporter>."
    echo "                    Valid options:  BrowserStack, teamcity, dots, progress, mocha"
    echo ""
    echo "  -? | -h | -help   to display this help information"
    echo ""
    echo "                    Specifying no option will perform a simple, single run of the test cases on"
    echo "                    the dominant set of browsers for the currently-detected active OS."
    echo ""
    exit 0
}

# Designed to determine which set of browsers should be available for local testing,
# based upon the current system OS.
get_OS ( ) {
  # Default value, since it's the most general case/configuration to detect.
  os_id="linux"

  # Subject to change with future improvements.
  if [[ "${OSTYPE}" = "darwin"* ]]; then
    os_id="mac"
  elif [[ "${OSTYPE}" = "msys" ]]; then
    os_id="win"
  elif [[ "${OSTYPE}" = "cygwin" ]]; then
    os_id="win"
  fi
}

get_browser_set_for_OS ( ) {
    if [ $os_id = "mac" ]; then
        BROWSERS="--browsers Firefox,Chrome,Safari"
    elif [ $os_id = "win" ]; then
        BROWSERS="--browsers Firefox,Chrome,IE,Edge"
    else
        BROWSERS="--browsers Firefox,Chrome"
    fi
}

# Defaults
get_OS
get_browser_set_for_OS

CONFIG=manual.conf.js  # TODO - get/make OS-specific version
DEBUG=false
FLAGS=

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -CI)
            CONFIG=CI.conf.js
            ;;
        -debug)
            # Disables the default 'run once, then done' configuration needed for CI.
            DEBUG=true
            FLAGS="--no-single-run $FLAGS"
            ;;
        -reporter)
            shift
            FLAGS="--reporters $1 $FLAGS"
            ;;
        -h)
            display_usage
            ;;
        -help)
            display_usage
            ;;
        -?)
            display_usage
            ;;
    esac
    shift # past argument
done

if [ $DEBUG = true ] && [ $CONFIG = CI.conf.js ]; then
    echo "-CI and -debug are not compatible!"
    exit 1
fi

if [ $CONFIG = CI.conf.js ]; then
    # If doing a CI run, use the file's default browser selection.
    BROWSERS=
fi

BASE_PATH=`dirname $BASH_SOURCE`
cd $BASE_PATH/../source

./build_recorder.sh

npm --no-color run modernizr -- -c unit_tests/modernizr.config.json -d unit_tests/modernizr.js
npm --no-color run karma -- start --log-level=debug $FLAGS $BROWSERS unit_tests/$CONFIG

CODE=$?

exit $CODE