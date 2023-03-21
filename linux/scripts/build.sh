#!/bin/bash

# Build Keyman for Linux: core ibus-keyman

# It must be run from the keyman/linux directory

set -e

BASEDIR=$(pwd)

INSTALLDIR=${INSTALLDIR:-"/usr/local"}

CONFIGUREONLY=${CONFIGUREONLY:="no"}
BUILDONLY=${BUILDONLY:="no"}

if [[ "${CONFIGUREONLY}" != "no" && "${BUILDONLY}" != "no" ]]; then
	echo "Only use one of CONFIGUREONLY and BUILDONLY"
	exit 1
fi

if [[ "${BUILDONLY}" == "no" ]]; then
	cd "$BASEDIR"
	../core/build.sh --no-tests configure:arch

	# While we can't pass extra parameters to the configure action
	# we need these extra commands
	cd ../core/build/arch/release
	echo "reconfiguring Keyman Core meson with prefix ${INSTALLDIR}"
	meson configure -Dprefix="${INSTALLDIR}" && ninja reconfigure

	# We need to build core first before we can configure ibus-keyman!
	cd "$BASEDIR"
	../core/build.sh build:arch

	cd "$BASEDIR/ibus-keyman"
	./build.sh clean configure -- --prefix="${INSTALLDIR}"
fi

if [[ "${CONFIGUREONLY}" == "no" ]]; then
	echo "building Keyman Core"
	cd "$BASEDIR"
	# May 2021: For now, running tests here as well. We could move this elsewhere
	# in the future if we want to split out the tests, but they run in a couple of seconds
	# at present.
	../core/build.sh build:arch test:arch

	echo "building ibus-keyman"
	cd "$BASEDIR/ibus-keyman"
	./build.sh build
fi
