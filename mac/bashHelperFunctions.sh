#!/usr/bin/env bash

# TODO: deprecate and replace with shellHelperFunctions.sh.

# The following allows coloring of warning and error lines, but only works if there's a
# terminal attached, so not on the build machine.
if ! [[ "$TERM" == "" || "$TERM" == "dumb" ]]; then
    ERROR_RED=$(tput setaf 1)
    WARNING_YELLOW=$(tput setaf 3)
    NORMAL=$(tput sgr0)
fi

fail() {
    FAILURE_MSG="$1"
    if [[ "$FAILURE_MSG" == "" ]]; then
        FAILURE_MSG="Unknown failure"
    fi
    echo "${ERROR_RED}$FAILURE_MSG${NORMAL}"
    exit 1
}

displayInfo() {
    if [ "$QUIET" != true ]; then
        while [[ $# -gt 0 ]] ; do
            echo $1
            shift # past argument
        done
    fi
}

assertFileExists() {
    if ! [ -f $1 ]; then
        fail "Build failed:  missing $1"
    fi
}

assertValidVersionNbr()
{
    if [[ "$1" == "" || ! "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        fail "Specified version not valid: '$1'. Version should be in the form Major.Minor.BuildCounter"
    fi
}