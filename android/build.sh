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
cd KMEA
./build.sh $@

if [ $? -ne 0 ]; then
    die "ERROR: KMEA/build.sh failed"
fi

cd ../KMAPro
./build.sh $@

if [ $? -ne 0 ]; then
    die "ERROR: KMAPro/build.sh failed"
fi

cd ../
