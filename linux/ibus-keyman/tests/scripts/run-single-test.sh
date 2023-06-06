#!/usr/bin/env bash
set -eu
TESTDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman/test_kmx

. "$(dirname "$0")"/test-helper.inc.sh

if [ -v KEYMAN_PKG_BUILD ]; then
  # During package builds we skip these tests that require to start ibus because
  # ibus requires to find /var/lib/dbus/machine-id or /etc/machine-id, otherwise it fails with:
  # "Bail out! IBUS-FATAL-WARNING: Unable to load /var/lib/dbus/machine-id: Failed to open file
  # “/var/lib/dbus/machine-id”: No such file or directory"
  echo "1..1"
  echo "ok 1 - Integration tests # SKIP on package build"
  exit 0
fi

if ! which Xvfb > /dev/null || ! which Xephyr > /dev/null || ! which metacity > /dev/null || ! which mutter > /dev/null; then
  echo "Please install Xvfb, Xephyr, metacity and mutter before running these tests!"
  exit 1
fi

function help() {
  echo "Usage:"
  echo "  $0 [--env <envfile>] [-k] [--tap] [--surrounding-text] [--no-surrounding-text] [--wayland|--x11] [--] TEST"
  echo
  echo "Arguments:"
  echo "  --help, -h, -?          Display this help"
  echo "  --verbose, -v           Run tests verbosely"
  echo "  --debug                 debug test logging output"
  echo "  -k                      passed to GLib testing framework"
  echo "  --tap                   output in TAP format. Passed to GLib testing framework"
  echo "  --surrounding-text      run tests with surrounding texts enabled"
  echo "  --no-surrounding-text   run tests without support for surrounding text"
  echo "  --wayland               run tests with Wayland"
  echo "  --x11                   run tests with X11"
  echo "  --env <envfile>         Name of the file containing environment variables to use"
  exit 0
}

function run_tests() {
  echo "# NOTE: When the tests fail check /tmp/ibus-engine-keyman.log and /tmp/ibus-daemon.log!"
  echo ""

  echo "# Starting tests..."
  # Note: -k and --tap are consumed by the GLib testing framework
  # shellcheck disable=SC2086
  "${G_TEST_BUILDDIR:-.}"/ibus-keyman-tests ${ARG_K-} ${ARG_TAP-} \
    ${ARG_VERBOSE-} ${ARG_DEBUG-} ${ARG_SURROUNDING_TEXT-} ${ARG_NO_SURROUNDING_TEXT-} \
    --directory "$TESTDIR" ${ARG_DISPLAY_SERVER} "$TESTFILE"
  echo "# Finished tests."
}

while (( $# )); do
  case $1 in
    --help|-h|-\?) help ;;
    -k) ARG_K=$1 ;;
    --tap) ARG_TAP=$1 ;;
    --surrounding-text) ARG_SURROUNDING_TEXT=$1 ;;
    --no-surrounding-text) ARG_NO_SURROUNDING_TEXT=$1 ;;
    --wayland) ARG_DISPLAY_SERVER=$1 ;;
    --x11) ARG_DISPLAY_SERVER=$1 ;;
    --verbose|-v) ARG_VERBOSE=--verbose;;
    --debug) ARG_DEBUG=--debug-log;;
    --env) shift ; ARG_ENV=$1 ;;
    --) shift ; TESTFILE=$1; break ;;
    *) echo "Error: Unexpected argument \"$1\". Exiting." ; exit 4 ;;
  esac
  shift || (echo "Error: The last argument is missing a value. Exiting."; false) || exit 5
done

# shellcheck source=/dev/null
. "$ARG_ENV"

run_tests
