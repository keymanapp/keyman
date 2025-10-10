# shellcheck shell=bash

function can_run_wayland() {
  local MUTTER_VERSION
  MUTTER_VERSION=$(mutter --version | head -1 | cut -f2 -d' ' | cut -f1 -d'.')
  if (( MUTTER_VERSION < 40 )); then
    return 1
  else
    return 0
  fi
}

function _generate_kmpjson() {
  local TESTDIR
  TESTDIR="$1"
  pushd "${TESTDIR}" > /dev/null || exit
  KMPFILE="${TESTDIR}/kmp.json"
  cat <<-EOF > "${KMPFILE}"
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
    keyboard=$(basename "${f}")
    keyboard="${keyboard%.*}"
    echo "      ," >> "${KMPFILE}"
    cat <<-EOF >> "${KMPFILE}"
      {
        "name": "${keyboard}",
        "description": "${keyboard}"
      }
EOF
  done
  cat <<-EOF >> "${KMPFILE}"
    ],
    "keyboards": [
EOF
  FIRST=true
  for f in k_*.kmx; do
    keyboard=$(basename "${f}")
    keyboard="${keyboard%.*}"
    if ${FIRST}; then
      FIRST=false
    else
      echo "    ," >> "${KMPFILE}"
    fi
    cat <<-EOF >> "${KMPFILE}"
    {
      "name": "${keyboard}",
      "id": "${keyboard}",
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
  echo "  ]" >> "${KMPFILE}"
  echo "}" >> "${KMPFILE}"
  popd > /dev/null || exit
}

function _link_test_keyboards() {
  KMX_TEST_DIR=$1
  TESTDIR=$2
  TESTBASEDIR=$3

  if [[ $(find "${KMX_TEST_DIR}/" -name k_\*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
    mkdir -p "$(realpath --canonicalize-missing "${TESTBASEDIR}")"
    rm -f "${TESTDIR}"
    ln -sf "$(realpath "${KMX_TEST_DIR}")" "${TESTDIR}"
  else
    echo "Can't find test kmx files in ${KMX_TEST_DIR}"
    exit 3
  fi
}

function _setup_init() {
  local ENV_FILE CLEANUP_FILE PID_FILE DISPLAY_SERVER
  ENV_FILE=$1
  CLEANUP_FILE=$2
  PID_FILE=$3
  DISPLAY_SERVER=$4

  if [[ -z "${TOP_SRCDIR:-}" ]]; then
    TOP_SRCDIR=${G_TEST_SRCDIR:-$(realpath "$(dirname "$0")/..")}/..
  fi
  if [[ -z "${TOP_BINDIR:-}" ]]; then
    TOP_BINDIR=${G_TEST_BUILDDIR:-$(realpath "$(dirname "$0/..")")}/..
  fi

  echo > "${ENV_FILE}"

  if [[ -f "${CLEANUP_FILE}" ]]; then
    # kill previous instances
    "$(dirname "$0")"/teardown-tests.sh "${CLEANUP_FILE}" "${DISPLAY_SERVER}" || true
  fi

  echo > "${CLEANUP_FILE}"
  echo > "${PID_FILE}"
  TEMP_DATA_DIR=$(mktemp --directory)
  echo "rm -rf \"${TEMP_DATA_DIR}\" || true # TEMP_DATA_DIR" >> "${CLEANUP_FILE}"

  COMMON_ARCH_DIR=
  [[ -d "${TOP_SRCDIR}"/../../core/build/arch ]] && COMMON_ARCH_DIR=${TOP_SRCDIR}/../../core/build/arch

  if [[ -d "${COMMON_ARCH_DIR}"/release ]]; then
    COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/release
  elif [[ -d "${COMMON_ARCH_DIR}"/debug ]]; then
    COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/debug
  else
    echo "Can't find neither ${COMMON_ARCH_DIR}/release nor ${COMMON_ARCH_DIR}/debug"
    exit 2
  fi

  export LD_LIBRARY_PATH=${COMMON_ARCH_DIR}/src:${LD_LIBRARY_PATH-}
  echo "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" >> "${ENV_FILE}"
}

function _setup_dbus_session() {
  local ENV_FILE CLEANUP_FILE
  ENV_FILE="$1"
  CLEANUP_FILE="$2"

  if [[ -z "${DOCKER_RUNNING:-}" ]] && [[ -z "${TEAMCITY_GIT_PATH:-}" ]] && ! pgrep -f jetbrains.buildServer.agent.AgentMain > /dev/null; then
    return
  fi

  if [[ ! -d "/run/user/${UID:-$(id -u)}" ]]; then
    # /run/user/$UID usually gets created by the session, but since we
    # don't start the regular UI we don't have a session. Therefore we
    # call the setupRuntimeDir.sh script to create the directory. The
    # script should contain the following commands:
    #  mkdir -p "/run/user/${SUDO_UID:-$(id -u)}"
    #  chmod 0700 "/run/user/${SUDO_UID:-$(id -u)}"
    #  chown "${SUDO_USER:-$(whoami)}" "/run/user/${SUDO_UID:-$(id -u)}"
    if [[ -f "/usr/local/bin/setupRuntimeDir.sh" ]]; then
      echo "# Creating /run/user/${UID:-$(id -u)}..."
      sudo /usr/local/bin/setupRuntimeDir.sh
    else
      echo "# WARNING: /run/user/${UID:-$(id -u)} is missing without having /usr/local/bin/setupRuntimeDir.sh, tests will likely fail"
    fi
  fi

  if [[ -z "${XDG_RUNTIME_DIR:-}" ]]; then
    export XDG_RUNTIME_DIR=/run/user/${UID:-$(id -u)}
    echo "export XDG_RUNTIME_DIR=/run/user/${UID:-$(id -u)}" >> "${ENV_FILE}"
  fi
  if [[ ! -d "${XDG_RUNTIME_DIR}" ]]; then
    echo "${XDG_RUNTIME_DIR} is missing, please create it"
    exit 1
  fi

  export NO_AT_BRIDGE=1
  echo "export NO_AT_BRIDGE=1" >> "${ENV_FILE}"

  if pgrep -u "${USER:-$(whoami)}" dbus-daemon > /dev/null; then
    # dbus-daemon is already running, skipping setup
    return
  fi

  echo "# Starting dbus session..."
  dbus-launch --auto-syntax >> "${ENV_FILE}"
  PID=$(pidof dbus-daemon)
  echo "kill -9 ${PID} || true # dbus-daemon" >> "${CLEANUP_FILE}"
}

function _setup_test_dbus_server() {
  local ENV_FILE CLEANUP_FILE
  ENV_FILE=$1
  CLEANUP_FILE=$2

  # Start test dbus server. This will create `/tmp/km-test-server.env`.
  "${TOP_BINDIR}/tests/dbus-test-server/km-dbus-test-server" &> /tmp/km-test-server.log &
  sleep 1

  cat /tmp/km-test-server.env >> "${ENV_FILE}"
  cat /tmp/km-test-server.env >> "${CLEANUP_FILE}"
  echo "${TOP_BINDIR}/tests/dbus-test-server/stop-test-server" >> "${CLEANUP_FILE}"

  # shellcheck disable=SC1091
  source /tmp/km-test-server.env
  # shellcheck disable=SC2154
  echo "# DBUS_SESSION_BUS_ADDRESS=${DBUS_SESSION_BUS_ADDRESS}"
}

function _setup_display_server() {
  local DISPLAY_SERVER ENV_FILE CLEANUP_FILE PID_FILE PID DISP_XVFB DISP_XEPHYR
  ENV_FILE=$1
  CLEANUP_FILE=$2
  PID_FILE=$3
  DISPLAY_SERVER=$4

  if [[ "${DISPLAY_SERVER}" == "--wayland" ]]; then
    if ! can_run_wayland; then
      # support for --headless got added in mutter 40.x
      echo "ERROR: mutter doesn't support running headless. Can't run Wayland tests."
      exit 7
    fi
    echo "Running on Wayland:"
    echo "Starting mutter..."
    TMPFILE=$(mktemp)
    # mutter-Message: 18:56:15.422: Using Wayland display name 'wayland-1'
    mutter --wayland --headless --no-x11 --virtual-monitor 1024x768 &> "${TMPFILE}" &
    PID=$!
    echo "kill -9 ${PID} &> /dev/null || true # mutter" >> "${CLEANUP_FILE}"
    echo "${PID} mutter" >> "${PID_FILE}"
    sleep 1s
    WAYLAND_DISPLAY=$(grep "Using Wayland display" "${TMPFILE}" | cut -d"'" -f2)
    export WAYLAND_DISPLAY
    rm "${TMPFILE}"
    echo "export WAYLAND_DISPLAY=\"${WAYLAND_DISPLAY}\"" >> "${ENV_FILE}"
  else
    echo "Running on X11:"
    while true; do
      echo "Starting Xvfb..."
      DISP_XVFB=${RANDOM}
      Xvfb -screen 0 1024x768x24 ":${DISP_XVFB}" &> /dev/null &
      PID=$!
      sleep 1
      if ps --no-headers --pid="${PID}" > /dev/null; then
        break
      fi
    done
    echo "kill -9 ${PID} &> /dev/null || true # Xvfb" >> "${CLEANUP_FILE}"
    echo "${PID} Xvfb" >> "${PID_FILE}"
    while true; do
      echo "Starting Xephyr..."
      DISP_XEPHYR=${RANDOM}
      DISPLAY=:${DISP_XVFB} Xephyr ":${DISP_XEPHYR}" -screen 1024x768 &> /dev/null &
      PID=$!
      sleep 1
      if ps --no-headers --pid="${PID}" > /dev/null; then
        break
      fi
    done
    echo "kill -9 ${PID} &> /dev/null || true # Xephyr" >> "${CLEANUP_FILE}"
    echo "${PID} Xephyr" >> "${PID_FILE}"
    echo "Starting metacity"
    metacity --display=":${DISP_XEPHYR}" &> /dev/null &
    PID=$!
    echo "kill -9 ${PID} &> /dev/null || true # metacity" >> "${CLEANUP_FILE}"
    echo "${PID} metacity" >> "${PID_FILE}"

    export DISPLAY=:${DISP_XEPHYR}
    echo "export DISPLAY=\"${DISPLAY}\"" >> "${ENV_FILE}"
  fi
}

function _setup_schema_and_gsettings() {
  local ENV_FILE
  ENV_FILE=$1

  # Install schema to temporary directory. This removes the build dependency on the keyman package.
  SCHEMA_DIR=${TEMP_DATA_DIR}/glib-2.0/schemas
  export XDG_DATA_DIRS=${TEMP_DATA_DIR}:${XDG_DATA_DIRS-}
  echo "export XDG_DATA_DIRS=${XDG_DATA_DIRS}" >> "${ENV_FILE}"

  export GSETTINGS_SCHEMA_DIR=${SCHEMA_DIR}
  echo "export GSETTINGS_SCHEMA_DIR=\"${SCHEMA_DIR}\"" >> "${ENV_FILE}"

  mkdir -p "${SCHEMA_DIR}"
  cp "${TOP_SRCDIR}"/../keyman-config/resources/com.keyman.gschema.xml "${SCHEMA_DIR}"/
  glib-compile-schemas "${SCHEMA_DIR}"

  # Ubuntu 18.04 Bionic doesn't have ibus-memconf, and glib is not compiled with the keyfile
  # backend enabled, so we just use the default backend. Otherwise we use the keyfile
  # store which interferes less when running on a dev machine.
  if [[ -f /usr/libexec/ibus-memconf ]]; then
    export GSETTINGS_BACKEND=keyfile
    echo "export GSETTINGS_BACKEND=\"${GSETTINGS_BACKEND}\"" >> "${ENV_FILE}"
    IBUS_CONFIG=--config=/usr/libexec/ibus-memconf
  fi
}

function _setup_ibus() {
  local ENV_FILE CLEANUP_FILE PID_FILE PID STANDALONE
  ENV_FILE=$1
  CLEANUP_FILE=$2
  PID_FILE=$3
  STANDALONE=${4:-}

  echo "Starting ibus-daemon..."
  #shellcheck disable=SC2086
  ibus-daemon ${ARG_VERBOSE-} --daemonize --panel=disable --address=unix:abstract="${TEMP_DATA_DIR}/test-ibus" ${IBUS_CONFIG-} &> /tmp/ibus-daemon.log

  PID=$(pgrep -f "${TEMP_DATA_DIR}/test-ibus")
  if [[ "${STANDALONE}" == "--standalone" ]] && [[ "${DOCKER_RUNNING:-false}" != "true" ]] && [[ -z "${TEAMCITY_GIT_PATH:-}" ]];  then
    # manual test run
    echo "if kill -9 ${PID} &> /dev/null; then ibus restart || ibus start -d; fi # ibus-daemon" >> "${CLEANUP_FILE}"
  else
    # test run as part of the build
    echo "kill -9 ${PID} &> /dev/null || true # ibus-daemon" >> "${CLEANUP_FILE}"
  fi
  echo "${PID} ibus-daemon" >> "${PID_FILE}"
  sleep 1s

  IBUS_ADDRESS=$(ibus address)
  export IBUS_ADDRESS

  echo "export IBUS_ADDRESS=\"${IBUS_ADDRESS}\"" >> "${ENV_FILE}"

  echo "Starting ibus-engine-keyman..."
  #shellcheck disable=SC2086
  "${TOP_BINDIR}/src/ibus-engine-keyman" --testing ${ARG_VERBOSE-} &> /tmp/ibus-engine-keyman.log &
  PID=$!
  echo "kill -9 ${PID} &> /dev/null || true # ibus-engine-keyman" >> "${CLEANUP_FILE}"
  echo "${PID} ibus-engine-keyman" >> "${PID_FILE}"
  sleep 1s
}

function setup() {
  local DISPLAY_SERVER ENV_FILE CLEANUP_FILE PID_FILE TESTBASEDIR TESTDIR STANDALONE
  DISPLAY_SERVER=$1
  ENV_FILE=$2
  CLEANUP_FILE=$3
  PID_FILE=$4
  STANDALONE=${5:-}

  _setup_init "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}" "${DISPLAY_SERVER}"

  TESTBASEDIR=${XDG_DATA_HOME:-${HOME}/.local/share}/keyman
  TESTDIR=${TESTBASEDIR}/test_kmx

  _link_test_keyboards "${TOP_SRCDIR}/../../common/test/keyboards/baseline" "${TESTDIR}" "${TESTBASEDIR}"

  _generate_kmpjson "${TESTDIR}"

  _setup_dbus_session "${ENV_FILE}" "${CLEANUP_FILE}"
  _setup_test_dbus_server "${ENV_FILE}" "${CLEANUP_FILE}"
  _setup_display_server "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}" "${DISPLAY_SERVER}"
  _setup_schema_and_gsettings "${ENV_FILE}"
  _setup_ibus "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}" "${STANDALONE}"
}

function setup_display_server_only() {
  local DISPLAY_SERVER ENV_FILE CLEANUP_FILE PID_FILE TESTBASEDIR TESTDIR
  DISPLAY_SERVER=$1
  ENV_FILE=$2
  CLEANUP_FILE=$3
  PID_FILE=$4

  _setup_init "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}" "${DISPLAY_SERVER}"
  _setup_display_server "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}" "${DISPLAY_SERVER}"
  _setup_schema_and_gsettings "${ENV_FILE}"
}

function cleanup() {
  local CLEANUP_FILE DISPLAY_SERVER
  CLEANUP_FILE=$1
  DISPLAY_SERVER=$2

  if [[ -f "${CLEANUP_FILE}" ]]; then
    echo
    echo "# Shutting down processes..."
    bash "${CLEANUP_FILE}" # &> /dev/null
    rm "${CLEANUP_FILE}"
    echo "# Finished shutdown of processes."
  fi

  if [[ "${DOCKER_RUNNING:-false}" == "true" ]]; then
    echo "# Copying log files..."
    # Note: build/tmp is build/docker-linux/<distro>/tmp on the host!
    BUILD_TMP="$(dirname "$0")/../../../build/tmp"
    mkdir -p "${BUILD_TMP}"
    [[ -f /tmp/ibus-engine-keyman.log ]] && mv /tmp/ibus-engine-keyman.log "${BUILD_TMP}/ibus-engine-keyman${DISPLAY_SERVER}.log"
    [[ -f /tmp/ibus-daemon.log ]] && mv /tmp/ibus-daemon.log "${BUILD_TMP}/ibus-daemon${DISPLAY_SERVER}.log"
    [[ -f /tmp/km-test-server.log ]] && mv /tmp/km-test-server.log "${BUILD_TMP}/km-test-server${DISPLAY_SERVER}.log"
    echo "# Finished copying log files"
  fi

  if [[ -z "${DOCKER_RUNNING:-}" ]] && [[ -z "${TEAMCITY_GIT_PATH:-}" ]] && ! pgrep -f jetbrains.buildServer.agent.AgentMain > /dev/null; then
    # Not running in Docker or TeamCity:
    # Killing the ibus-daemon that we started for testing might have caused
    # some other ibus processes that belong to the real ibus daemon to
    # be killed as well, leaving us with no way to type.
    # So we kill the remaining lonely ibus-daemon and start ibus again.
    if [[ "$(ibus engine)" == ""  ]]; then
      echo "# Killing and restarting ibus-daemon"
      kill -9 "$(pgrep -u "${USER:-$(whoami)}" ibus-daemon)"
      ibus start -d
    fi
  fi
}

function exit_on_package_build() {
  if [[ -v KEYMAN_PKG_BUILD ]]; then
    # Skip setup during package builds - can't run headless and we won't
    # run the other tests anyway
    exit 0
  fi
}

function _get_missing_processes() {
  local PID_FILE MISSING_PROCS PID LINE
  PID_FILE=$1
  MISSING_PROCS=""

  while read -r LINE; do
    if [[ -z "${LINE}" ]]; then
      continue
    fi
    PID=$(echo "${LINE}" | cut -d' ' -f1)
    if ! ps --no-headers --pid="${PID}" > /dev/null; then
      MISSING_PROCS="${MISSING_PROCS}    $(echo "${LINE}" | cut -d' ' -f2)"
      break
    fi
  done < "${PID_FILE}"
}

function check_processes_running() {
  local DISPLAY_SERVER ENV_FILE CLEANUP_FILE PID_FILE MISSING_PROCS TEST_NAME
  DISPLAY_SERVER=$1
  ENV_FILE=$2
  CLEANUP_FILE=$3
  PID_FILE=$4
  TEST_NAME=$5
  MISSING_PROCS=$(_get_missing_processes "${PID_FILE}")

  if [[ "${MISSING_PROCS}" != "" ]]; then
    echo "# Some background processes no longer running. Restarting..."
    { echo "Some background processes no longer running (running ${TEST_NAME}):" ; \
      echo "${MISSING_PROCS}" ; \
      echo "Restarting..." ; } >> /tmp/debug.output
    mv /tmp/ibus-engine-keyman.log{,"-${TEST_NAME}-$(date -Iseconds)"}
    cleanup "${CLEANUP_FILE}" "${DISPLAY_SERVER}" &> /dev/null
    setup "${DISPLAY_SERVER}" "${ENV_FILE}" "${CLEANUP_FILE}" "${PID_FILE}" &> /dev/null
  fi

  if [[ "$(_get_missing_processes "$PID_FILE")" != "" ]]; then
    echo "# WARNING: Expected background processes are still missing after restart: ${MISSING_PROCS}"
    echo "# Maybe an old process is still running?"
  fi
}
