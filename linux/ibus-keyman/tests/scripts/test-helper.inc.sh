#!/usr/bin/env bash

function can_run_wayland() {
  local MUTTER_VERSION
  MUTTER_VERSION=$(mutter --version | head -1 | cut -f2 -d' ' | cut -f1 -d'.')
  if (( MUTTER_VERSION < 40 )); then
    return 1
  else
    return 0
  fi
}

function generate_kmpjson() {
  local TESTDIR
  TESTDIR="$1"
  pushd "$TESTDIR" > /dev/null || exit
  KMPFILE="${TESTDIR}/kmp.json"
  cat <<-EOF > "$KMPFILE"
  {
    "system": {
      "keymanDeveloperVersion": "10.0.1099.0",
      "fileVersion": "7.0"
    },
    "info": {
      "name": {
        "description": "Test Keyboards"
      },
      "version": {
        "description": "0.0"
      }
    },
    "files": [
      {
        "name": "kmp.json",
        "description": "Package information (JSON)"
      }
EOF
  for f in k_*.kmx; do
    keyboard=$(basename "$f")
    keyboard="${keyboard%.*}"
    echo "      ," >> "$KMPFILE"
    cat <<-EOF >> "$KMPFILE"
      {
        "name": "$keyboard",
        "description": "$keyboard"
      }
EOF
  done
  cat <<-EOF >> "$KMPFILE"
    ],
    "keyboards": [
EOF
  FIRST=true
  for f in k_*.kmx; do
    keyboard=$(basename "$f")
    keyboard="${keyboard%.*}"
    if $FIRST; then
      FIRST=false
    else
      echo "    ," >> "$KMPFILE"
    fi
    cat <<-EOF >> "$KMPFILE"
    {
      "name": "$keyboard",
      "id": "$keyboard",
      "version": "0.0",
      "languages": [
        {
          "name": "Undetermined",
          "id": "und"
        }
      ]
    }
EOF
  done
  echo "  ]" >> "$KMPFILE"
  echo "}" >> "$KMPFILE"
  popd > /dev/null || exit
}

function link_test_keyboards() {
  KMX_TEST_DIR=$1
  TESTDIR=$2
  TESTBASEDIR=$3

  if [[ $(find "${KMX_TEST_DIR}/" -name k_\*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
    mkdir -p "$(realpath --canonicalize-missing "$TESTBASEDIR")"
    rm -f "$TESTDIR"
    ln -sf "$(realpath "${KMX_TEST_DIR}")" "$TESTDIR"
  else
    echo "Can't find test kmx files in ${KMX_TEST_DIR}"
    exit 3
  fi
}

function setup() {
  local DISPLAY_SERVER ENV_FILE PID_FILE TOP_SRCDIR TOP_BINDIR TESTBASEDIR TESTDIR
  DISPLAY_SERVER=$1
  ENV_FILE=$2
  PID_FILE=$3

  TOP_SRCDIR=${G_TEST_SRCDIR:-$(realpath "$(dirname "$0")/..")}/..
  TOP_BINDIR=${G_TEST_BUILDDIR:-$(realpath "$(dirname "$0/..")")}/..
  TESTBASEDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman
  TESTDIR=${TESTBASEDIR}/test_kmx

  echo > "$ENV_FILE"

  if [ -f "$PID_FILE" ]; then
    # kill previous instances
    "$(dirname "$0")"/teardown-tests.sh "$PID_FILE"
  fi

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

  link_test_keyboards "${TOP_SRCDIR}/../../common/test/keyboards/baseline" "$TESTDIR" "$TESTBASEDIR"

  generate_kmpjson "$TESTDIR"

  # Start test dbus server
  "${TOP_BINDIR}/tests/km-dbus-test-server" &> /dev/null &
  sleep 1
  source /tmp/km-test-server.env
  cat /tmp/km-test-server.env >> "$ENV_FILE"
  cat /tmp/km-test-server.env >> "$PID_FILE"
  echo "${TOP_BINDIR}/tests/stop-test-server" >> "$PID_FILE"

  if [ "$DISPLAY_SERVER" == "wayland" ]; then
    if ! can_run_wayland; then
      # support for --headless got added in mutter 40.x
      echo "ERROR: mutter doesn't support running headless. Can't run Wayland tests."
      exit 7
    fi
    echo "Running on Wayland:"
    echo "Starting mutter..."
    TMPFILE=$(mktemp)
    # mutter-Message: 18:56:15.422: Using Wayland display name 'wayland-1'
    mutter --wayland --headless --no-x11 --virtual-monitor 1024x768 &> "$TMPFILE" &
    echo "kill -9 $! || true" >> "$PID_FILE"
    sleep 1s
    export WAYLAND_DISPLAY
    WAYLAND_DISPLAY=$(grep "Using Wayland display" "$TMPFILE" | cut -d"'" -f2)
    rm "$TMPFILE"
    echo "export WAYLAND_DISPLAY=\"$WAYLAND_DISPLAY\"" >> "$ENV_FILE"
  else
    echo "Running on X11:"
    echo "Starting Xvfb..."
    Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
    echo "kill -9 $! || true" >> "$PID_FILE"
    sleep 1
    echo "Starting Xephyr..."
    DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
    echo "kill -9 $! || true" >> "$PID_FILE"
    sleep 1
    echo "Starting metacity"
    metacity --display=:32 &> /dev/null &
    echo "kill -9 $! || true" >> "$PID_FILE"

    export DISPLAY=:32
    echo "export DISPLAY=\"$DISPLAY\"" >> "$ENV_FILE"
  fi

  # Install schema to temporary directory. This removes the build dependency on the keyman package.
  SCHEMA_DIR=$TEMP_DATA_DIR/glib-2.0/schemas
  export XDG_DATA_DIRS=$TEMP_DATA_DIR:${XDG_DATA_DIRS-}
  echo "export XDG_DATA_DIRS=$XDG_DATA_DIRS" >> "$ENV_FILE"

  export GSETTINGS_SCHEMA_DIR=${SCHEMA_DIR}
  echo "export GSETTINGS_SCHEMA_DIR=\"$SCHEMA_DIR\"" >> "$ENV_FILE"

  mkdir -p "$SCHEMA_DIR"
  cp "${TOP_SRCDIR}"/../keyman-config/com.keyman.gschema.xml "$SCHEMA_DIR"/
  glib-compile-schemas "$SCHEMA_DIR"

  export LD_LIBRARY_PATH=${COMMON_ARCH_DIR}/src:${LD_LIBRARY_PATH-}
  echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> "$ENV_FILE"

  # Ubuntu 18.04 Bionic doesn't have ibus-memconf, and glib is not compiled with the keyfile
  # backend enabled, so we just use the default backend. Otherwise we use the keyfile
  # store which interferes less when running on a dev machine.
  if [ -f /usr/libexec/ibus-memconf ]; then
    export GSETTINGS_BACKEND=keyfile
    echo "export GSETTINGS_BACKEND=\"$GSETTINGS_BACKEND\"" >> "$ENV_FILE"
    IBUS_CONFIG=--config=/usr/libexec/ibus-memconf
  fi

  #shellcheck disable=SC2086
  ibus-daemon ${ARG_VERBOSE-} --daemonize --panel=disable --address=unix:abstract="${TEMP_DATA_DIR}/test-ibus" ${IBUS_CONFIG-} &> /tmp/ibus-daemon.log
  echo "kill -9 $! || true" >> "$PID_FILE"
  sleep 1s

  IBUS_ADDRESS=$(ibus address)
  export IBUS_ADDRESS

  echo "export IBUS_ADDRESS=\"$IBUS_ADDRESS\"" >> "$ENV_FILE"

  echo "# DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS"
  #shellcheck disable=SC2086
  "${TOP_BINDIR}/src/ibus-engine-keyman" --testing ${ARG_VERBOSE-} &> /tmp/ibus-engine-keyman.log &
  echo "kill -9 $! || true" >> "$PID_FILE"
  sleep 1s
}

function cleanup() {
  local PID_FILE
  PID_FILE=$1

  if [ -f "$PID_FILE" ]; then
    echo
    echo "# Shutting down processes..."
    bash "$PID_FILE" > /dev/null 2>&1
    rm "$PID_FILE"
    echo "# Finished shutdown of processes."
  fi
}
