#!/bin/bash

BASEDIR=$(realpath $(dirname $0))
TESTDIR=${XDG_DATA_HOME:-$HOME/.local/share}/keyman/test_kmx

if [ "$DISPLAY" == ":0" ]; then
  echo "Please start xephyr and set the DISPLAY environment variable before running these tests."
  exit 1
fi

if [ ! -d $TESTDIR ]; then
  if [ -d $BASEDIR/../../../common/core/desktop/build/arch/debug ]; then
    ln -sf $(realpath $BASEDIR/../../../common/core/desktop/build/arch/debug/tests/unit/kmx) $TESTDIR
  elif [ -d $BASEDIR/../../../common/core/desktop/build/arch/release ]; then
    ln -sf $(realpath $BASEDIR/../../../common/core/desktop/build/arch/release/tests/unit/kmx) $TESTDIR
  else
    echo "Can't find kmx files in common/core/desktop/build/arch/*/tests/unit/kmx"
    exit 2
  fi
fi

if [ $# -gt 0 ]; then
  TESTFILES=$@
else
  pushd $TESTDIR
  TESTFILES=(*.kmx)
  popd
fi

export GSETTINGS_BACKEND=keyfile

ibus-daemon --panel=disable --config=/usr/libexec/ibus-memconf > /tmp/ibus-daemon.log 2>&1 &
../src/ibus-engine-keyman > /tmp/ibus-engine-keyman.log 2>&1 &
sleep 1s

./ibus-keyman-tests -k --tap --directory $TESTDIR "${TESTFILES[@]}"
