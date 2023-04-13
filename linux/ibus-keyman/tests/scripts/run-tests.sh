#!/usr/bin/env bash

TOP_SRCDIR=${top_srcdir:-$(realpath "$(dirname "$0")/..")}
TESTDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman/test_kmx

. "$(dirname "$0")"/test-helper.sh

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

function cleanup() {
  if [ -f "$PID_FILE" ]; then
    echo
    echo "# Shutting down processes..."
    bash "$PID_FILE" > /dev/null 2>&1
    rm "$PID_FILE"
    echo "# Finished shutdown of processes."
  fi
}

function help() {
  echo "Usage:"
  echo "  $0 [-k] [--tap] [--surrounding-text] [--no-surrounding-text] [--no-wayland] [--no-x11] [[--] TEST...]"
  echo
  echo "Arguments:"
  echo "  --help, -h, -?          Display this help"
  echo "  --verbose, -v           Run tests verbosely"
  echo "  --debug                 debug test logging output"
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

  echo > "$PID_FILE"
  TEMP_DATA_DIR=$(mktemp --directory)
  echo "rm -rf ${TEMP_DATA_DIR} || true" >> "$PID_FILE"

  COMMON_ARCH_DIR=
  [ -d "${TOP_SRCDIR}"/../../core/build/arch ] && COMMON_ARCH_DIR=${TOP_SRCDIR}/../../core/build/arch

  if [ -d "${COMMON_ARCH_DIR}"/release ]; then
    COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/release
  elif [ -d "${COMMON_ARCH_DIR}"/debug ]; then
    COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/debug
  else
    echo "Can't find neither ${COMMON_ARCH_DIR}/release nor ${COMMON_ARCH_DIR}/debug"
    exit 2
  fi

  # TODO: we are borrowing the KMX tests out of core; these should be shared in common
  # to avoid deep links like this in future
  KMX_TEST_DIR="${TOP_SRCDIR}"/../../common/test/keyboards/baseline

  if [ ! -d "$TESTDIR" ] || ! [[ $(find "${TESTDIR}/" -name \*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
    if [[ $(find "${KMX_TEST_DIR}/" -name \*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
      mkdir -p "$(realpath --canonicalize-missing "$TESTDIR"/..)"
      ln -sf "$(realpath "${KMX_TEST_DIR}")" "$TESTDIR"
    else
      echo "Can't find kmx files in ${KMX_TEST_DIR}"
      exit 3
    fi
  fi

  echo "# NOTE: When the tests fail check /tmp/ibus-engine-keyman.log and /tmp/ibus-daemon.log!"
  echo ""

  if [ "$DISPLAY_SERVER" == "wayland" ]; then
    if ! can_run_wayland; then
      # support for --headless got added in mutter 40.x
      echo "ERROR: mutter doesn't support running headless. Can't run Wayland tests."
      exit 7
    fi
    echo "# Running on Wayland..."
    TMPFILE=$(mktemp)
    # mutter-Message: 18:56:15.422: Using Wayland display name 'wayland-1'
    mutter --wayland --headless --no-x11 --virtual-monitor 1024x768 &> "$TMPFILE" &
    echo "kill -9 $! || true" >> "$PID_FILE"
    sleep 1s
    export WAYLAND_DISPLAY
    WAYLAND_DISPLAY=$(cat "$TMPFILE" | grep "Using Wayland display" | cut -d"'" -f2)
    rm "$TMPFILE"
  else
    echo "# Starting Xvfb..."
    Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
    echo "kill -9 $! || true" >> "$PID_FILE"
    sleep 1
    echo "# Starting Xephyr..."
    DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
    echo "kill -9 $! || true" >> "$PID_FILE"
    sleep 1
    echo "# Starting metacity"
    metacity --display=:32 &> /dev/null &
    echo "kill -9 $! || true" >> "$PID_FILE"

    export DISPLAY=:32
  fi

  # Install schema to temporary directory. This removes the build dependency on the keyman package.
  SCHEMA_DIR=$TEMP_DATA_DIR/glib-2.0/schemas
  export XDG_DATA_DIRS=$TEMP_DATA_DIR:$XDG_DATA_DIRS

  mkdir -p "$SCHEMA_DIR"
  cp "${TOP_SRCDIR}"/../keyman-config/com.keyman.gschema.xml "$SCHEMA_DIR"/
  glib-compile-schemas "$SCHEMA_DIR"

  if [ $# -gt 0 ]; then
    TESTFILES=($@)
  else
    pushd "$TESTDIR" > /dev/null || exit
    TESTFILES=(*.kmx)
    popd > /dev/null || exit
  fi

  export LD_LIBRARY_PATH=${COMMON_ARCH_DIR}/src:$LD_LIBRARY_PATH

  # Ubuntu 18.04 Bionic doesn't have ibus-memconf, and glib is not compiled with the keyfile
  # backend enabled, so we just use the default backend. Otherwise we use the keyfile
  # store which interferes less when running on a dev machine.
  if [ -f /usr/libexec/ibus-memconf ]; then
    export GSETTINGS_BACKEND=keyfile
    IBUS_CONFIG=--config=/usr/libexec/ibus-memconf
  fi

  ibus-daemon "${ARG_VERBOSE-}" --panel=disable ${IBUS_CONFIG-} &> /tmp/ibus-daemon.log &
  echo "kill -9 $! || true" >> "$PID_FILE"
  sleep 1s

  ../src/ibus-engine-keyman "${ARG_VERBOSE-}" &> /tmp/ibus-engine-keyman.log &
  echo "kill -9 $! || true" >> "$PID_FILE"
  sleep 1s

  echo "# Starting tests..."
  # Note: -k and --tap are consumed by the GLib testing framework
  "${G_TEST_BUILDDIR:-.}"/ibus-keyman-tests "${ARG_K-}" "${ARG_TAP-}" \
    "${ARG_VERBOSE-}" "${ARG_DEBUG-}" "${ARG_SURROUNDING_TEXT-}" "${ARG_NO_SURROUNDING_TEXT-}" \
    --directory "$TESTDIR" --"${DISPLAY_SERVER}" "${TESTFILES[@]}"
  echo "# Finished tests."

  cleanup
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
    --) shift && break ;;
    *) echo "Error: Unexpected argument \"$1\". Exiting." ; exit 4 ;;
  esac
  shift || (echo "Error: The last argument is missing a value. Exiting."; false) || exit 5
done

if ! can_run_wayland; then
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

echo > "$PID_FILE"
trap cleanup EXIT SIGINT

if [ "$USE_WAYLAND" == "1" ]; then
  run_tests wayland "$@"
fi

if [ "$USE_X11" == "1" ]; then
  run_tests x11 "$@"
fi
