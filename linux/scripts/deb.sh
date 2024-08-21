#!/bin/bash

# Build Debian source packages of Keyman

# It must be run from the keyman/linux directory

# parameters: ./deb.sh

set -e

BASEDIR=$(pwd)
#echo "basedir is $BASEDIR"

mkdir -p builddebs

# make the source packages
cd builddebs
vers=$(ls ../dist/keyman_*.orig.tar.gz)
#echo "vers1:${vers}"
vers=${vers##*_}
#echo "vers2:${vers}"
vers=${vers%*.orig.tar.gz}
#echo "vers3:${vers}"
cp -a "../dist/keyman_${vers}.orig.tar.gz" .
tar xfz "keyman_${vers}.orig.tar.gz"
cp -a ../../debian "keyman-${vers}"
cd "keyman-${vers}"
dch -v "${vers}-1" "local build"
echo "keyman-${vers}"
dch --release --distribution unstable ""
debuild -d -S -sa -Zxz -us -uc
cd "$BASEDIR/builddebs"
rm -rf "keyman-${vers}"
cd "$BASEDIR"
