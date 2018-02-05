#! /bin/bash

# A simple utility script to facilitate our different modes for unit-testing KMW.
# It's rigged to be callable 

display_usage ( ) {
    echo "test.sh [-CI | -debug | -? | -h | -help]"
    echo
    echo "  -CI               to run unit tests in CI mode on BrowserStack."
    echo "                    This requires credentials to be set in environment variables - see "
    echo "                    https://stackoverflow.com/questions/32450546/hiding-browserstack-key-in-karma"
    echo ""
    echo "  -debug            to establish a Karma server that facilitates unit test debugging"
    echo ""
    echo "  -? | -h | -help   to display this help information"
    echo ""
    echo "                    Specifying no option will perform a simple, single run of the test cases."
    echo ""
    exit 0
}

# Defaults
CONFIG=manual.conf.js

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -CI)
            CONFIG=CI.conf.js
            ;;
        -debug)
            CONFIG=debug.conf.js
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

npm run karma start unit_tests/$CONFIG
CODE=$?

exit $CODE