#!/bin/bash

BASEDIR=$(realpath $(dirname $0))
TESTDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman/test_kmx

if ! which Xvfb > /dev/null || ! which Xephyr > /dev/null || ! which metacity > /dev/null; then
  echo "Please install Xvfb, Xephyr and metacity before running these tests!"
  exit 1
fi

function cleanup() {
  if [ -f /tmp/ibus-keyman-test-pids ]; then
    echo
    echo "Shutting down processes..."
    bash /tmp/ibus-keyman-test-pids > /dev/null 2>&1
    rm /tmp/ibus-keyman-test-pids
    echo "Finished shutdown of processes."
  fi
}

echo > /tmp/ibus-keyman-test-pids

trap cleanup EXIT SIGINT

echo "Starting Xvfb..."
Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
echo "kill -9 $!" >> /tmp/ibus-keyman-test-pids
sleep 1
echo "Starting Xephyr..."
DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
echo "kill -9 $!" >> /tmp/ibus-keyman-test-pids
sleep 1
echo "Starting metacity"
metacity --display=:32 &> /dev/null &
echo "kill -9 $!" >> /tmp/ibus-keyman-test-pids

export DISPLAY=:32

if [ ! -d $TESTDIR ]; then
  KMXDIR=
  [ -d $BASEDIR/../../../common/core/desktop/build/arch ] && KMXDIR=$BASEDIR/../../../common/core/desktop/build/arch
  [ -d $BASEDIR/../../keyboardprocessor/arch ] && KMXDIR=$BASEDIR/../../keyboardprocessor/arch
  if [[ $(wc -l $KMXDIR/*/tests/unit/kmx/*.kmx 2>/dev/null) > 0 ]]; then
    mkdir -p $TESTDIR
    ln -sf $(realpath $KMXDIR/*/tests/unit/kmx) $TESTDIR
  else
    echo "Can't find kmx files in common/core/desktop/build/arch/*/tests/unit/kmx or linux/keyboardprocessor/arch/*/tests/unit/kmx"
    exit 2
  fi
fi

if [ $# -gt 0 ]; then
  [ "$1" == "--" ] && shift
  TESTFILES=($@)
else
  pushd $TESTDIR > /dev/null
  TESTFILES=(*.kmx)
  popd > /dev/null
fi

export GSETTINGS_BACKEND=keyfile

ibus-daemon --panel=disable --config=/usr/libexec/ibus-memconf &> /tmp/ibus-daemon.log &
echo "kill -9 $!" >> /tmp/ibus-keyman-test-pids
../src/ibus-engine-keyman &> /tmp/ibus-engine-keyman.log &
echo "kill -9 $!" >> /tmp/ibus-keyman-test-pids
sleep 1s

echo "Starting tests..."
./ibus-keyman-tests -k --tap --directory $TESTDIR "${TESTFILES[@]}"
echo "Finished tests."
