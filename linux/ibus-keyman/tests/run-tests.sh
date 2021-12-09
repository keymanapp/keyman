#!/bin/bash

TOP_SRCDIR=${top_srcdir:-$(realpath $(dirname $0)/..)}
BASEDIR=$(realpath $(dirname $0))
TESTDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman/test_kmx
PID_FILE=/tmp/ibus-keyman-test-pids

if [ -v DEB_BUILD_MULTIARCH ]; then
  # During package builds we skip these tests that require to start ibus because
  # ibus requires to find /var/lib/dbus/machine-id or /etc/machine-id, otherwise it fails with:
  # "Bail out! IBUS-FATAL-WARNING: Unable to load /var/lib/dbus/machine-id: Failed to open file
  # “/var/lib/dbus/machine-id”: No such file or directory"
  echo "1..1"
  echo "ok 1 - Integration tests # SKIP on package build"
  exit 0
fi

if ! which Xvfb > /dev/null || ! which Xephyr > /dev/null || ! which metacity > /dev/null; then
  echo "Please install Xvfb, Xephyr and metacity before running these tests!"
  exit 1
fi

function cleanup() {
  if [ -f $PID_FILE ]; then
    echo
    echo "Shutting down processes..."
    bash $PID_FILE > /dev/null 2>&1
    rm $PID_FILE
    echo "Finished shutdown of processes."
  fi
}

function help() {
  echo "Usage:"
  echo "  $0 [-k] [--tap] [--surrounding-text] [--no-surrounding-text] [[--] TEST...]"
  echo
  echo "Arguments:"
  echo "  --help, -h, -?          Display this help"
  echo "  -k                      passed to GLib testing framework"
  echo "  --tap                   output in TAP format. Passed to GLib testing framework"
  echo "  --surrounding-text      run tests with surrounding texts enabled"
  echo "  --no-surrounding-text   run tests without support for surrounding text"
  echo
  echo "If no TESTs are specified then all tests are run."
  echo "If neither --surrounding-text nor --no-surrounding-text are specified then the tests run with both settings."
  exit 0
}

while (( $# )); do
  case $1 in
    --help|-h|-\?) help ;;
    -k) ARG_K=$1 ;;
    --tap) ARG_TAP=$1 ;;
    --surrounding-text) ARG_SURROUNDING_TEXT=$1 ;;
    --no-surrounding-text) ARG_NO_SURROUNDING_TEXT=$1 ;;
    --) shift && break ;;
    *) echo "Error: Unexpected argument \"$1\". Exiting." ; exit 4 ;;
  esac
  shift || (echo "Error: The last argument is missing a value. Exiting."; false) || exit 5
done

echo > $PID_FILE
trap cleanup EXIT SIGINT

TEMP_DATA_DIR=$(mktemp --directory)
echo "rm -rf ${TEMP_DATA_DIR}" >> $PID_FILE

COMMON_ARCH_DIR=
[ -d ${TOP_SRCDIR}/../../common/core/desktop/build/arch ] && COMMON_ARCH_DIR=${TOP_SRCDIR}/../../common/core/desktop/build/arch
[ -d ${TOP_SRCDIR}/../keyboardprocessor/arch ] && COMMON_ARCH_DIR=${TOP_SRCDIR}/../keyboardprocessor/arch

if [ -d ${COMMON_ARCH_DIR}/release ]; then
  COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/release
elif [ -d ${COMMON_ARCH_DIR}/debug ]; then
  COMMON_ARCH_DIR=${COMMON_ARCH_DIR}/debug
else
  echo "Can't find neither ${COMMON_ARCH_DIR}/release nor ${COMMON_ARCH_DIR}/debug"
  exit 2
fi

if [ ! -d $TESTDIR ] || ! [[ $(ls -x ${TESTDIR}/*.kmx 2>/dev/null | wc -l) > 0 ]]; then
  if [[ $(ls -x ${COMMON_ARCH_DIR}/tests/unit/kmx/*.kmx 2>/dev/null | wc -l) > 0 ]]; then
    mkdir -p $(realpath --canonicalize-missing $TESTDIR/..)
    ln -sf $(realpath ${COMMON_ARCH_DIR}/tests/unit/kmx) $TESTDIR
  else
    echo "Can't find kmx files in ${COMMON_ARCH_DIR}/tests/unit/kmx"
    exit 3
  fi
fi

echo "Starting Xvfb..."
Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
echo "kill -9 $!" >> $PID_FILE
sleep 1
echo "Starting Xephyr..."
DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
echo "kill -9 $!" >> $PID_FILE
sleep 1
echo "Starting metacity"
metacity --display=:32 &> /dev/null &
echo "kill -9 $!" >> $PID_FILE

export DISPLAY=:32

# Install schema to temporary directory. This removes the build dependency on the keyman package.
SCHEMA_DIR=$TEMP_DATA_DIR/glib-2.0/schemas
export XDG_DATA_DIRS=$TEMP_DATA_DIR:$XDG_DATA_DIRS

mkdir -p $SCHEMA_DIR
cp ${TOP_SRCDIR}/../keyman-config/com.keyman.gschema.xml $SCHEMA_DIR/
glib-compile-schemas $SCHEMA_DIR

if [ $# -gt 0 ]; then
  TESTFILES=($@)
else
  pushd $TESTDIR > /dev/null
  TESTFILES=(*.kmx)
  popd > /dev/null
fi

export LD_LIBRARY_PATH=${COMMON_ARCH_DIR}/src:$LD_LIBRARY_PATH

# Ubuntu 18.04 Bionic doesn't have ibus-memconf, and glib is not compiled with the keyfile
# backend enabled, so we just use the default backend. Otherwise we use the keyfile
# store which interferes less when running on a dev machine.
if [ -f /usr/libexec/ibus-memconf ]; then
  export GSETTINGS_BACKEND=keyfile
  IBUS_CONFIG=--config=/usr/libexec/ibus-memconf
fi

ibus-daemon --panel=disable ${IBUS_CONFIG-} &> /tmp/ibus-daemon.log &
echo "kill -9 $!" >> $PID_FILE
../src/ibus-engine-keyman &> /tmp/ibus-engine-keyman.log &
echo "kill -9 $!" >> $PID_FILE
sleep 1s

echo "Starting tests..."
# Note: -k and --tap are consumed by the GLib testing framework
./ibus-keyman-tests ${ARG_K-} ${ARG_TAP-} ${ARG_SURROUNDING_TEXT-} ${ARG_NO_SURROUNDING_TEXT-} --directory $TESTDIR "${TESTFILES[@]}"
echo "Finished tests."
