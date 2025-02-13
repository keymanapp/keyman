#!/usr/bin/env bash
set -eu

TOP_SRCDIR=${top_srcdir:-$(realpath "$(dirname "$0")/../..")}
TESTBASEDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman
TESTDIR=${TESTBASEDIR}/test_kmx
CLEANUP_FILE=/tmp/ibus-keyman-test-cleanup
PID_FILE=/tmp/ibus-keyman-test.pids
ENV_FILE=/tmp/keyman-env.txt

. "$(dirname "$0")"/test-helper.inc.sh

local_cleanup() {
  cleanup "$CLEANUP_FILE"
}

if [ -v KEYMAN_PKG_BUILD ]; then
  # During package builds we skip these tests that require to start ibus because
  # ibus requires to find /var/lib/dbus/machine-id or /etc/machine-id, otherwise it fails with:
  # "Bail out! IBUS-FATAL-WARNING: Unable to load /var/lib/dbus/machine-id: Failed to open file
  # “/var/lib/dbus/machine-id”: No such file or directory"
  echo "1..0 # SKIP on package build"
  exit 0
fi

if ! which Xvfb > /dev/null || ! which Xephyr > /dev/null || ! which metacity > /dev/null || ! which mutter > /dev/null; then
  echo "Please install Xvfb, Xephyr, metacity and mutter before running these tests!"
  exit 1
fi

function help() {
  echo "Usage:"
  echo "  $0 [-k] [--tap] [--surrounding-text] [--no-surrounding-text] [--no-wayland] [--no-x11] [[--] TEST...]"
  echo
  echo "Arguments:"
  echo "  --help, -h, -?          Display this help"
  echo "  --verbose, -v           Run tests verbosely"
  echo "  --debug                 debug test logging output"
  echo "  --remote-debug          start gdbserver after setup but prior to running the tests"
  echo "  -k                      passed to GLib testing framework"
  echo "  --tap                   output in TAP format. Passed to GLib testing framework"
  echo "  --surrounding-text      run tests with surrounding texts enabled"
  echo "  --no-surrounding-text   run tests without support for surrounding text"
  echo "  --no-wayland            don't run tests with Wayland"
  echo "  --no-x11                don't run tests with X11"
  echo
  echo "If no TESTs are specified then all tests are run."
  echo "If neither --surrounding-text nor --no-surrounding-text are specified then the tests run with both settings."
  exit 0
}

function run_tests() {
  DISPLAY_SERVER=$1
  shift

  if [ -d "$(dirname "$0")/../../../build/$(arch)/debug" ]; then
    CONFIG=debug
  elif [ -d "$(dirname "$0")/../../../build/$(arch)/release" ]; then
    CONFIG=release
  else
    echo "Cannot find ../../../build/$(arch)/debug or ../../../build/$(arch)/release"
    exit 9
  fi

  G_TEST_BUILDDIR="$(dirname "$0")/../../../build/$(arch)/${CONFIG}/tests"

  setup "$DISPLAY_SERVER" "$ENV_FILE" "$CLEANUP_FILE" "$PID_FILE" --standalone

  if [[ "${DOCKER_RUNNING:-false}" == "true" ]]; then
    echo "# NOTE: When the tests fail check ibus-engine-keyman.log, ibus-daemon.log and km-test-server.log in build/docker-linux/tmp/!"
  else
    echo "# NOTE: When the tests fail check /tmp/ibus-engine-keyman.log, /tmp/ibus-daemon.log and /tmp/km-test-server.log!"
  fi
  echo ""

  if [ $# -gt 0 ]; then
    #shellcheck disable=SC2206
    TESTFILES=($@)
  else
    pushd "$TESTDIR" > /dev/null || exit
    TESTFILES=(*.kmx)
    popd > /dev/null || exit
  fi

  echo "# Starting tests..."
  # shellcheck disable=SC1090
  source "$ENV_FILE"
  echo "DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS"

  if [[ -n ${REMOTE_DEBUG:-} ]]; then
    echo "===> Now attach debugger to ${REMOTE_HOST}"
  fi

  # Note: -k and --tap are consumed by the GLib testing framework
  #shellcheck disable=SC2068 # we want to split array elements!
  #shellcheck disable=SC2086
  ${REMOTE_DEBUG:-} "${G_TEST_BUILDDIR:-../../build/$(arch)/${CONFIG}/tests}/ibus-keyman-tests" ${ARG_K-} ${ARG_TAP-} \
    ${ARG_VERBOSE-} ${ARG_DEBUG-} ${ARG_SURROUNDING_TEXT-} ${ARG_NO_SURROUNDING_TEXT-} \
    --directory "$TESTDIR" "${DISPLAY_SERVER}" ${TESTFILES[@]}
  echo "# Finished tests."

  cleanup "$CLEANUP_FILE"
}

USE_WAYLAND=1
USE_X11=1

while (( $# )); do
  case $1 in
    --help|-h|-\?) help ;;
    -k) ARG_K=$1 ;;
    --tap) ARG_TAP=$1 ;;
    --surrounding-text) ARG_SURROUNDING_TEXT=$1 ;;
    --no-surrounding-text) ARG_NO_SURROUNDING_TEXT=$1 ;;
    --no-wayland) USE_WAYLAND=0;;
    --no-x11) USE_X11=0;;
    --verbose|-v) ARG_VERBOSE=--verbose;;
    --debug) ARG_DEBUG=--debug-log;;
    --remote-debug) REMOTE_HOST="$(ip route get 8.8.8.8 | sed -E 's/.*src (\S+) .*/\1/;t;d'):2345" && REMOTE_DEBUG="gdbserver ${REMOTE_HOST}";;
    --) shift && break ;;
    *) echo "Error: Unexpected argument \"$1\". Exiting." ; exit 4 ;;
  esac
  shift || (echo "Error: The last argument is missing a value. Exiting."; false) || exit 5
done

if ! can_run_wayland && [ "$USE_WAYLAND" == "0" ]; then
  # support for --headless got added in mutter 40.x
  echo "# WARNING: mutter doesn't support running headless. Skipping Wayland tests."
  USE_WAYLAND=0
  if [ "$USE_X11" == "0" ]; then
    echo "ERROR: no tests to run. Can't run Wayland tests, and --no-x11 is specified."
    exit 8
  fi
fi

if [ "$USE_WAYLAND" == "0" ] && [ "$USE_X11" == "0" ]; then
  echo "ERROR: I'll have to run somewhere. Specifying both --no-wayland and --no-x11 is not allowed."
  exit 6
fi

G_TEST_BUILDDIR="${G_TEST_BUILDDIR:-../../build/$(arch)/debug/tests}"
if [ ! -f "${G_TEST_BUILDDIR}/ibus-keyman-tests" ]; then
  G_TEST_BUILDDIR="${G_TEST_BUILDDIR:-../../build/$(arch)/release/tests}"
fi

echo > "$CLEANUP_FILE"
echo > "$PID_FILE"
trap local_cleanup EXIT SIGINT

if [ "$USE_WAYLAND" == "1" ]; then
  run_tests --wayland "$@"
fi

if [ "$USE_X11" == "1" ]; then
  run_tests --x11 "$@"
fi
