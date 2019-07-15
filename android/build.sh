#!/bin/sh
# Build Keyman Engine Android and KMAPro

die () {
    echo
    echo "$*"
    echo
    exit 1
}

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

echo Build KMEA and KMAPro:

# Building Keyman Engine for Android

cd KMEA
./build.sh "$@"

if [ $? -ne 0 ]; then
    die "ERROR: KMEA/build.sh failed"
fi

# Building Keyman for Android

cd ../KMAPro
./build.sh "$@"

if [ $? -ne 0 ]; then
    die "ERROR: KMAPro/build.sh failed"
fi

cd ../

# Building OEM apps

if [ ! -z "$RELEASE_OEM" ]; then
  pushd ../oem/firstvoices/android
  ./build.sh -lib-nobuild "$@"

  if [ $? -ne 0 ]; then
    die "ERROR: oem/firstvoices/android/build.sh failed"
  fi
fi
