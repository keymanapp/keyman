#!/bin/bash

# Build Keyman for Linux: keyboardprocessor ibus-keyman
# If BUILD_LEGACY is set instead, additionally build KMFL in the required order:
#   kmflcomp libkmfl ibus-kmfl

# It must be run from the keyman/linux directory

set -e

BASEDIR=`pwd`

INSTALLDIR=${INSTALLDIR:-"/usr/local"}

CONFIGUREONLY=${CONFIGUREONLY:="no"}
BUILDONLY=${BUILDONLY:="no"}

legacy_projects=""
if [ -n "$BUILD_LEGACY" ]; then
    legacy_projects="kmflcomp libkmfl ibus-kmfl"
fi
projects="ibus-keyman"

if [[ "${CONFIGUREONLY}" != "no" && "${BUILDONLY}" != "no" ]]; then
	echo "Only use one of CONFIGUREONLY and BUILDONLY"
	exit 1
fi

if [[ "${BUILDONLY}" == "no" ]]; then
	../core/build.sh --target-path "$BASEDIR/keyboardprocessor" configure:arch

	cd keyboardprocessor/arch/release
	echo "reconfiguring keyboardprocessor meson with prefix ${INSTALLDIR}"
	meson configure -Dprefix=${INSTALLDIR} && ninja reconfigure
	cd $BASEDIR
fi

if [[ "${CONFIGUREONLY}" == "no" ]]; then
	echo "building keyboardprocessor"
	# May 2021: For now, running tests here as well. We could move this elsewhere
	# in the future if we want to split out the tests, but they run in a couple of seconds
	# at present.
	../core/build.sh --target-path "$BASEDIR/keyboardprocessor" build:arch test:arch
fi

function buildproject() {
	local proj=$1
	local subdir=$2

	echo "buildproject: proj=$proj, subdir=$subdir; pwd=$(pwd)"
	if [ ! -f ${subdir}${proj}/configure ]; then
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
			../${subdir}$proj/configure KEYMAN_PROC_CFLAGS="-I\$(top_builddir)/../keyboardprocessor/arch/release/include -I\$(top_builddir)/../../common/include -I\$(top_builddir)/../../core/include" \
				CPPFLAGS="-I\$(top_builddir)/../build-kmflcomp -I\$(top_builddir)/../build-libkmfl" \
				KEYMAN_PROC_LIBS="-L`pwd`/../build-libkmfl/src -L`pwd`/../keyboardprocessor/arch/release/src -lkmnkbp0" \
				LDFLAGS="-L`pwd`/../build-kmflcomp/src -L`pwd`/../build-libkmfl/src" --prefix=${INSTALLDIR} --libexecdir=${INSTALLDIR}/lib/ibus
		else	# install ibus-kmfl and ibus-keyman into ibus
			../${subdir}$proj/configure KEYMAN_PROC_CFLAGS="-I\$(top_builddir)/../keyboardprocessor/arch/release/include -I\$(top_builddir)/../../common/include -I\$(top_builddir)/../../core/include" \
				CPPFLAGS="-I\$(top_builddir)/../build-kmflcomp -I\$(top_builddir)/../build-libkmfl" \
				LDFLAGS="-L`pwd`/../build-kmflcomp/src -L`pwd`/../build-libkmfl/src" \
				KEYMAN_PROC_LIBS="-L`pwd`/../build-libkmfl/src -L`pwd`/../keyboardprocessor/arch/release/src -lkmnkbp0" \
				--prefix=${INSTALLDIR} --libexecdir=${INSTALLDIR}/lib/ibus --datadir=/usr/share
		fi
		if [ -d ../${subdir}$proj/include ]; then
			echo "copying $proj include files to kmfl dir"
			mkdir kmfl
			cp -a ../${subdir}$proj/include/*.h kmfl
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
}

for proj in $legacy_projects; do
    buildproject $proj legacy/
done

for proj in $projects; do
    buildproject $proj
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
