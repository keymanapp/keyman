#!/bin/bash

function help() {
  echo "Usage:"
  echo "  $0 [--env <envfile>] [-k] [--tap] [--] TESTFILE [TESTARGS]"
  echo
  echo "Arguments:"
  echo "  --help, -h, -?          Display this help"
  echo "  --verbose, -v           Run tests verbosely"
  echo "  --debug                 debug test logging output"
  echo "  -k                      passed to GLib testing framework"
  echo "  --tap                   output in TAP format. Passed to GLib testing framework"
  echo "  --env <envfile>         Name of the file containing environment variables to use"
  exit 0
}

function run_tests() {
  # Output these lines to stderr - the first line on stdout has to be the TAP version number
  # which running ${TESTFILE} outputs
  echo "# NOTE: When the tests fail check /tmp/ibus-engine-keyman.log and /tmp/ibus-daemon.log!" >&2
  echo "" >&2

  echo "# Starting tests..." >&2

  # Note: -k and --tap are consumed by the GLib testing framework
  # shellcheck disable=SC2086
  "${TESTFILE}" ${ARG_K-} ${ARG_TAP-} ${ARG_VERBOSE-} ${ARG_DEBUG-} "$@"
  echo "# Finished tests."
}

while (( $# )); do
  case $1 in
    --help|-h|-\?) help ;;
    -k) ARG_K=$1 ;;
    --tap) ARG_TAP=$1 ;;
    --verbose|-v) ARG_VERBOSE=--verbose;;
    --debug) ARG_DEBUG=--debug-log;;
    --env) shift ; ARG_ENV=$1 ;;
    --) shift ; TESTFILE=$1; shift ; break ;;
    *) echo "Error: Unexpected argument \"$1\". Exiting." ; exit 4 ;;
  esac
  shift || (echo "Error: The last argument is missing a value. Exiting."; false) || exit 5
done

# shellcheck source=/dev/null
. "$ARG_ENV"

run_tests "$@"
