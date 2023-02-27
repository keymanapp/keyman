#!/usr/bin/env bash
DISPLAY_SERVER=$1
ENV_FILE=$2
PID_FILE=$3

TOP_SRCDIR=${G_TEST_SRCDIR:-$(realpath "$(dirname "$0")/..")}/..
TOP_BINDIR=${G_TEST_BUILDDIR:-$(realpath "$(dirname "$0/..")")}/..
TESTDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman/test_kmx

. "$(dirname "$0")/"/test-helper.sh

echo > "$ENV_FILE"

if [ -f "$PID_FILE" ]; then
  # kill previous instances
  "$(dirname "$0")"/teardown-tests.sh "$PID_FILE"
fi

echo > "$PID_FILE"
TEMP_DATA_DIR=$(mktemp --directory)
echo "rm -rf ${TEMP_DATA_DIR}" >> "$PID_FILE"

COMMON_ARCH_DIR=
[ -d "${TOP_SRCDIR}"/../../core/build/arch ] && COMMON_ARCH_DIR=${TOP_SRCDIR}/../../core/build/arch
[ -d "${TOP_SRCDIR}"/../keyboardprocessor/arch ] && COMMON_ARCH_DIR=${TOP_SRCDIR}/../keyboardprocessor/arch

if [ -d "${COMMON_ARCH_DIR}"/release ]; then
  COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/release
elif [ -d "${COMMON_ARCH_DIR}"/debug ]; then
  COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/debug
else
  echo "Can't find neither ${COMMON_ARCH_DIR}/release nor ${COMMON_ARCH_DIR}/debug"
  exit 2
fi

if [ ! -d "$TESTDIR" ] || ! [[ $(find "${TESTDIR}/" -name k_\*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
  if [[ $(find "${COMMON_ARCH_DIR}/tests/unit/kmx/" -name k_\*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
    mkdir -p "$(realpath --canonicalize-missing "$TESTDIR"/..)"
    ln -sf "$(realpath "${KMX_TEST_DIR}")" "$TESTDIR"
  else
    echo "Can't find kmx files in ${KMX_TEST_DIR}"
    exit 3
  fi
fi

if [ "$DISPLAY_SERVER" == "wayland" ]; then
  if ! can_run_wayland; then
    # support for --headless got added in mutter 40.x
    echo "ERROR: mutter doesn't support running headless. Can't run Wayland tests."
    exit 7
  fi
  echo "Running on Wayland..."
  TMPFILE=$(mktemp)
  # mutter-Message: 18:56:15.422: Using Wayland display name 'wayland-1'
  mutter --wayland --headless --no-x11 --virtual-monitor 1024x768 &> "$TMPFILE" &
  echo "kill -9 $!" >> "$PID_FILE"
  sleep 1s
  export WAYLAND_DISPLAY
  WAYLAND_DISPLAY=$(grep "Using Wayland display" "$TMPFILE" | cut -d"'" -f2)
  rm "$TMPFILE"
  echo "export WAYLAND_DISPLAY=\"$WAYLAND_DISPLAY\"" >> "$ENV_FILE"
else
  echo "Starting Xvfb..."
  Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
  echo "kill -9 $!" >> "$PID_FILE"
  sleep 1
  echo "Starting Xephyr..."
  DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
  echo "kill -9 $!" >> "$PID_FILE"
  sleep 1
  echo "Starting metacity"
  metacity --display=:32 &> /dev/null &
  echo "kill -9 $!" >> "$PID_FILE"

  export DISPLAY=:32
  echo "export DISPLAY=\"$DISPLAY\"" >> "$ENV_FILE"
fi

  # Install schema to temporary directory. This removes the build dependency on the keyman package.
  SCHEMA_DIR=$TEMP_DATA_DIR/glib-2.0/schemas
  export XDG_DATA_DIRS=$TEMP_DATA_DIR:$XDG_DATA_DIRS
  echo "export XDG_DATA_DIRS=\"$XDG_DATA_DIRS\"" >> "$ENV_FILE"

  mkdir -p "$SCHEMA_DIR"
  cp "${TOP_SRCDIR}"/../keyman-config/com.keyman.gschema.xml "$SCHEMA_DIR"/
  glib-compile-schemas "$SCHEMA_DIR"

  export LD_LIBRARY_PATH=${COMMON_ARCH_DIR}/src:$LD_LIBRARY_PATH
  echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\"" >> "$ENV_FILE"

  # Ubuntu 18.04 Bionic doesn't have ibus-memconf, and glib is not compiled with the keyfile
  # backend enabled, so we just use the default backend. Otherwise we use the keyfile
  # store which interferes less when running on a dev machine.
  if [ -f /usr/libexec/ibus-memconf ]; then
    export GSETTINGS_BACKEND=keyfile
    echo "export GSETTINGS_BACKEND=\"$GSETTINGS_BACKEND\"" >> "$ENV_FILE"
    IBUS_CONFIG=--config=/usr/libexec/ibus-memconf
  fi

  ibus-daemon "${ARG_VERBOSE-}" --daemonize --panel=disable ${IBUS_CONFIG-} &> /tmp/ibus-daemon.log
  echo "kill -9 $!" >> "$PID_FILE"
  sleep 1s

  IBUS_ADDRESS=$(ibus address)
  export IBUS_ADDRESS

  echo "export IBUS_ADDRESS=\"$IBUS_ADDRESS\"" >> "$ENV_FILE"

  "${TOP_BINDIR}"/src/ibus-engine-keyman "${ARG_VERBOSE-}" &> /tmp/ibus-engine-keyman.log &
  echo "kill -9 $!" >> "$PID_FILE"
  sleep 1s
