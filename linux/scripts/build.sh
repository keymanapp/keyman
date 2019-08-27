#!/bin/bash

# Build KMFL in the required order: keyboardprocessor kmflcomp libkmfl ibus-kmfl ibus-keyman

# It must be run from the keyman/linux directory

set -e

BASEDIR=`pwd`

INSTALLDIR=${INSTALLDIR:-"/usr/local"}

CONFIGUREONLY=${CONFIGUREONLY:="no"}
BUILDONLY=${BUILDONLY:="no"}

if [[ "${CONFIGUREONLY}" != "no" && "${BUILDONLY}" != "no" ]]; then
	echo "Only use one of CONFIGUREONLY and BUILDONLY"
	exit 1
fi

if [[ "${BUILDONLY}" == "no" ]]; then
	if [ ! -d keyboardprocessor ]; then
		meson ../common/engine/keyboardprocessor keyboardprocessor
	fi
	cd keyboardprocessor
	echo "reconfiguring keyboardprocessor meson with prefix ${INSTALLDIR}"
	meson configure -Dprefix=${INSTALLDIR} && ninja reconfigure
	cd $BASEDIR
fi

if [[ "${CONFIGUREONLY}" == "no" ]]; then
	echo "building keyboardprocessor"
	cd keyboardprocessor
	ninja
	cd $BASEDIR
fi

for proj in kmflcomp libkmfl ibus-kmfl ibus-keyman; do
	if [ ! -f $proj/configure ]; then
		echo "$proj is not set up with autoreconf. First run 'make reconf' or 'make devreconf'"
		exit 1
	fi
	if [[ "${BUILDONLY}" == "no" ]]; then
		rm -rf build-$proj
	fi
	mkdir -p build-$proj
	cd build-$proj
	if [[ "${BUILDONLY}" == "no" ]]; then
		echo "Configuring $proj"
		if [[ "${INSTALLDIR}" == "/tmp/kmfl" ]]; then # don't install ibus-kmfl or ibus-keyman into ibus
			../$proj/configure KEYMAN_PROC_CFLAGS="-I\$(top_builddir)/../keyboardprocessor/include -I\$(top_builddir)/../../common/engine/keyboardprocessor/include" \
				CPPFLAGS="-I\$(top_builddir)/../build-kmflcomp -I\$(top_builddir)/../build-libkmfl" \
				KEYMAN_PROC_LIBS="-L`pwd`/../build-libkmfl/src -L`pwd`/../keyboardprocessor/src -lkmnkbp0" \
				LDFLAGS="-L`pwd`/../build-kmflcomp/src -L`pwd`/../build-libkmfl/src" --prefix=${INSTALLDIR} --libexecdir=${INSTALLDIR}/lib/ibus
		else	# install ibus-kmfl and ibus-keyman into ibus
			../$proj/configure KEYMAN_PROC_CFLAGS="-I\$(top_builddir)/../keyboardprocessor/include -I\$(top_builddir)/../../common/engine/keyboardprocessor/include" \
				CPPFLAGS="-I\$(top_builddir)/../build-kmflcomp -I\$(top_builddir)/../build-libkmfl" \
				LDFLAGS="-L`pwd`/../build-kmflcomp/src -L`pwd`/../build-libkmfl/src" \
				KEYMAN_PROC_LIBS="-L`pwd`/../build-libkmfl/src -L`pwd`/../keyboardprocessor/src -lkmnkbp0" \
				--prefix=${INSTALLDIR} --libexecdir=${INSTALLDIR}/lib/ibus --datadir=/usr/share
		fi
		if [ -d ../$proj/include ]; then
			echo "copying $proj include files to kmfl dir"
			mkdir kmfl
			cp -a ../$proj/include/*.h kmfl
		fi
	fi
	if [[ "${CONFIGUREONLY}" == "no" ]]; then
		if [ ! -f config.h ]; then
			echo "$proj has not been configured before building. First run 'make configure'"
			exit 1
		fi
		echo "Building $proj"
		make
	fi
	cd $BASEDIR
done

if [[ "${CONFIGUREONLY}" == "no" ]]; then
	cd keyman-config
	if [ ! -f keyman_config/version.py ]; then
		echo "The version of keyman-config has not been configured before building. First run 'make reconf'"
	fi
	make clean
	make
	cd $BASEDIR
fi
