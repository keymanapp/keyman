#!/bin/bash

# Build Keyman for Linux: keyboardprocessor ibus-keyman

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
	../core/build.sh -t keyboardprocessor configure

	cd keyboardprocessor/arch/release
	echo "reconfiguring keyboardprocessor meson with prefix ${INSTALLDIR}"
	meson configure -Dprefix="${INSTALLDIR}" && ninja reconfigure

	cd "$BASEDIR/ibus-keyman"
	./build.sh configure -- -Dprefix="${INSTALLDIR}"
fi

if [[ "${CONFIGUREONLY}" == "no" ]]; then
	echo "building keyboardprocessor"
	# May 2021: For now, running tests here as well. We could move this elsewhere
	# in the future if we want to split out the tests, but they run in a couple of seconds
	# at present.
	../core/build.sh -t keyboardprocessor build tests

	echo "building ibus-keyman"
	cd "$BASEDIR/ibus-keyman"
	./build.sh build
fi
