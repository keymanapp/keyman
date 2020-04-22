#! /bin/bash

# A testing script designed for automating in-browser testing.
# It's designed to be called by this folder's ./test.sh.

display_usage ( ) {
    echo "browser_test.sh os [-CI | -debug | -? | -h | -help] [-reporter <reporter>]"
    echo
    echo "  os                Should be one of the following:  win, mac, linux"
    echo ""
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

os_id=$1

get_browser_set_for_OS ( ) {
    if [ $os_id = "mac" ]; then
        BROWSERS="--browsers Firefox,Chrome,Safari"
    elif [ $os_id = "win" ]; then
        BROWSERS="--browsers Chrome"
    else
        BROWSERS="--browsers Firefox,Chrome"
    fi
}

# Defaults
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

npm --no-color run karma -- start --log-level=debug $FLAGS $BROWSERS unit_tests/in_browser/$CONFIG

CODE=$?

exit $CODE
