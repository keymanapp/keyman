#!/bin/bash

# Build Keyman for Linux: keyboardprocessor ibus-keyman

# It must be run from the keyman/linux directory

set -e

BASEDIR=$(pwd)

INSTALLDIR=${INSTALLDIR:-"/usr/local"}

CONFIGUREONLY=${CONFIGUREONLY:="no"}
BUILDONLY=${BUILDONLY:="no"}

projects="ibus-keyman"

if [[ "${CONFIGUREONLY}" != "no" && "${BUILDONLY}" != "no" ]]; then
	echo "Only use one of CONFIGUREONLY and BUILDONLY"
	exit 1
fi

if [[ "${BUILDONLY}" == "no" ]]; then
	../core/build.sh --target-path "$BASEDIR/keyboardprocessor" configure:arch

	cd keyboardprocessor/arch/release
	echo "reconfiguring keyboardprocessor meson with prefix ${INSTALLDIR}"
	meson configure -Dprefix="${INSTALLDIR}" && ninja reconfigure
	cd "$BASEDIR"
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
	if [ ! -f "${subdir}${proj}/configure" ]; then
		echo "$proj is not set up with autoreconf. First run 'make reconf' or 'make devreconf'"
		exit 1
	fi
	if [[ "${BUILDONLY}" == "no" ]]; then
		rm -rf "build-$proj"
	fi
	mkdir -p "build-$proj"
	cd "build-$proj"
	if [[ "${BUILDONLY}" == "no" ]]; then
		echo "Configuring $proj"
		if [[ "${INSTALLDIR}" == "/tmp/keyman" ]]; then # don't install ibus-keyman into ibus
			"../${subdir}$proj/configure" KEYMAN_PROC_CFLAGS="-I\$(top_builddir)/../keyboardprocessor/arch/release/include -I\$(top_builddir)/../../common/include -I\$(top_builddir)/../../core/include" \
				CPPFLAGS="-I\$(top_builddir)/../build-keymancomp -I\$(top_builddir)/../build-libkeyman" \
				KEYMAN_PROC_LIBS="-L$(pwd)/../build-libkeyman/src -L$(pwd)/../keyboardprocessor/arch/release/src -lkmnkbp0" \
				LDFLAGS="-L$(pwd)/../build-keymancomp/src -L$(pwd)/../build-libkeyman/src" --prefix="${INSTALLDIR}" --libexecdir="${INSTALLDIR}/lib/ibus"
		else	# install ibus-keyman into ibus
			"../${subdir}$proj/configure" KEYMAN_PROC_CFLAGS="-I\$(top_builddir)/../keyboardprocessor/arch/release/include -I\$(top_builddir)/../../common/include -I\$(top_builddir)/../../core/include" \
				CPPFLAGS="-I\$(top_builddir)/../build-keymancomp -I\$(top_builddir)/../build-libkeyman" \
				LDFLAGS="-L$(pwd)/../build-keymancomp/src -L$(pwd)/../build-libkeyman/src" \
				KEYMAN_PROC_LIBS="-L$(pwd)/../build-libkeyman/src -L$(pwd)/../keyboardprocessor/arch/release/src -lkmnkbp0" \
				--prefix="${INSTALLDIR}" --libexecdir="${INSTALLDIR}/lib/ibus" --datadir=/usr/share
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
	cd "$BASEDIR"
}

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
	cd "$BASEDIR"
fi
