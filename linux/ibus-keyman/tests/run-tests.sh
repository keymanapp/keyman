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

echo > $PID_FILE
trap cleanup EXIT SIGINT

TEMP_DATA_DIR=$(mktemp --directory)
echo "rm -rf ${TEMP_DATA_DIR}" >> $PID_FILE

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

if [ ! -d $TESTDIR ]; then
  if [[ $(ls -l ${COMMON_ARCH_DIR}/tests/unit/kmx/*.kmx 2>/dev/null | wc -l) > 0 ]]; then
    mkdir -p $(realpath --canonicalize-missing $TESTDIR/..)
    ln -sf $(realpath ${COMMON_ARCH_DIR}/tests/unit/kmx) $TESTDIR
  else
    echo "Can't find kmx files in ${COMMON_ARCH_DIR}/tests/unit/kmx"
    exit 3
  fi
fi

[ "$1" == "-k" ] && ARG_K="-k" && shift
[ "$1" == "--tap" ] && ARG_TAP="--tap" && shift

if [ $# -gt 0 ]; then
  [ "$1" == "--" ] && shift
  TESTFILES=($@)
else
  pushd $TESTDIR > /dev/null
  TESTFILES=(*.kmx)
  popd > /dev/null
fi

export GSETTINGS_BACKEND=keyfile
export LD_LIBRARY_PATH=${COMMON_ARCH_DIR}/src:$LD_LIBRARY_PATH

# Ubuntu 18.04 Bionic doesn't have ibus-memconf
[ -f /usr/libexec/ibus-memconf ] && IBUS_CONFIG=--config=/usr/libexec/ibus-memconf

ibus-daemon --panel=disable ${IBUS_CONFIG-} &> /tmp/ibus-daemon.log &
echo "kill -9 $!" >> $PID_FILE
../src/ibus-engine-keyman &> /tmp/ibus-engine-keyman.log &
echo "kill -9 $!" >> $PID_FILE
sleep 1s

echo "Starting tests..."
./ibus-keyman-tests ${ARG_K-} ${ARG_TAP-} --directory $TESTDIR "${TESTFILES[@]}"
echo "Finished tests."
