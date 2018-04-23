#!/bin/sh

# Build KMFL in the required order: kmflcomp libkmfl ibus-kmfl
# install each to temp install dir INSTALLDIR which is /tmp/kmfl by default
#   so that libkmfl and ibus-kmfl can use what they need that have been installed

# It must be run from the keyman/linux directory


BASEDIR=`pwd`

INSTALLDIR=${INSTALLDIR:-"/tmp/kmfl"}

# autoreconf the projects
#for proj in kmflcomp libkmfl ibus-kmfl; do
#	cd $proj
#	autoreconf
#	cd $BASEDIR
#done

for proj in kmflcomp libkmfl ibus-kmfl; do
	rm -rf build-$proj
	mkdir build-$proj
	cd build-$proj
	CPPFLAGS="-I${INSTALLDIR}/include" LDFLAGS="-L${INSTALLDIR}/lib" ../$proj/configure --prefix=${INSTALLDIR} && make && make install
	cd $BASEDIR
done
