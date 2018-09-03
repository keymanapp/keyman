#!/bin/bash

# Build KMFL in the required order: kmflcomp libkmfl ibus-kmfl
# install each to temp install dir INSTALLDIR which is /tmp/kmfl by default
#   so that libkmfl and ibus-kmfl can use what they need that have been installed

# It must be run from the keyman/linux directory

# To install the ibus-kmfl xml file and icons to /usr/share so that ibus
# will use ibus-kmfl you must run SUDOINSTALL="yes" ./build.sh
# This will install everything else to /usr/local

set -e

BASEDIR=`pwd`

SUDOINSTALL=${SUDOINSTALL:-"no"}

INSTALLDIR=${INSTALLDIR:-"/tmp/kmfl"}

if [ "$1" == "dist" ]; then
	rm -rf dist
	mkdir -p ../dist
fi

# autoreconf the projects
for proj in kmflcomp libkmfl ibus-kmfl; do
	cd $proj
	echo "Reconfiguring $proj"
	autoreconf -if
	if [ "$1" == "dist" ]; then
		mkdir -p ../dist
		rm -rf ../build-$proj
		mkdir -p ../build-$proj
		cd ../build-$proj
		../$proj/configure
		make dist
		mv *.tar.gz ../dist
	fi
	cd $BASEDIR
done

if [ "$1" == "dist" ]; then
	cd keyman-config
	rm -rf dist
	python3 setup.py egg_info -b.`date -u +"%Y%m%d%H%M%S"` sdist
	cp dist/*.tar.gz ../dist
	exit 0
fi


if [[ "${SUDOINSTALL}" == "uninstall" ]]
then
	echo "putting ibus-kmfl package component file back"
	sudo mv /usr/share/doc/ibus-kmfl/kmfl.xml /usr/share/ibus/component/
	exit 0
fi

if [ -f "/usr/share/ibus/component/kmfl.xml" ] && [ "${SUDOINSTALL}" == "yes" ]; then
	if grep -Fq "/usr/lib/ibus" /usr/share/ibus/component/kmfl.xml
	then
		echo "component file is ibus-kmfl package version so move it"
		echo "run 'SUDOINSTALL=uninstall ./build.sh' to put it back"
		sudo mv /usr/share/ibus/component/kmfl.xml /usr/share/doc/ibus-kmfl/
	else
		echo "component file is local one so overwrite it"
	fi
fi

for proj in kmflcomp libkmfl ibus-kmfl; do
	rm -rf build-$proj
	mkdir build-$proj
	cd build-$proj
	if [[ "${SUDOINSTALL}" == "yes" ]]
	then
		echo "doing sudo install of $proj"
		../$proj/configure CPPFLAGS="-I$/usr/local/include" LDFLAGS="-L/usr/local/lib" --libexecdir=/usr/local/lib/ibus --datadir=/usr/share
		make
		sudo make install
	else
		../$proj/configure CPPFLAGS="-I${INSTALLDIR}/include" LDFLAGS="-L${INSTALLDIR}/lib" --prefix=${INSTALLDIR} --libexecdir=${INSTALLDIR}/lib/ibus
		make
		make install
	fi
	cd $BASEDIR
done
