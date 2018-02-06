#! /bin/bash

# A simple utility script to facilitate our different modes for unit-testing KMW.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
    echo "test.sh [-CI | -debug | -? | -h | -help] [-r <reporter>]"
    echo
    echo "  -CI               to run unit tests in CI mode on BrowserStack."
    echo "                    This requires credentials to be set in environment variables - see "
    echo "                    https://stackoverflow.com/questions/32450546/hiding-browserstack-key-in-karma"
    echo ""
    echo "  -debug            to establish a Karma server that facilitates unit test debugging"
    echo "                    Not compatible with -CI."
    echo ""
    echo "  -r                sets the test engine to utilize the specified <reporter>."
    echo "                    Valid options:  BrowserStack, teamcity, dots, progress, mocha"
    echo ""
    echo "  -? | -h | -help   to display this help information"
    echo ""
    echo "                    Specifying no option will perform a simple, single run of the test cases."
    echo ""
    exit 0
}

# Defaults
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
        -r)
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

npm run modernizr -- -c unit_tests/modernizr.config.json -d unit_tests/modernizr.js
npm run karma -- start $FLAGS unit_tests/$CONFIG

CODE=$?

exit $CODE