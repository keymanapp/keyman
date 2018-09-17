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
	if [[ "${SUDOINSTALL}" == "yes" ]]; then
		echo "doing sudo install of $proj"
		../$proj/configure CPPFLAGS="-I$/usr/local/include" LDFLAGS="-L/usr/local/lib" --libexecdir=/usr/local/lib/ibus --datadir=/usr/share
		make
		sudo make install
	elif [[ "${SUDOINSTALL}" == "uninstall" ]]; then
		echo "doing sudo uninstall of $proj"
		../$proj/configure CPPFLAGS="-I$/usr/local/include" LDFLAGS="-L/usr/local/lib" --libexecdir=/usr/local/lib/ibus --datadir=/usr/share
		sudo make uninstall
	else
		echo "doing /tmp install of $proj"
		../$proj/configure CPPFLAGS="-I${INSTALLDIR}/include" LDFLAGS="-L${INSTALLDIR}/lib" --prefix=${INSTALLDIR} --libexecdir=${INSTALLDIR}/lib/ibus
		make
		make install
	fi
	cd $BASEDIR
done

cd keyman-config
echo "SUDOINSTALL: ${SUDOINSTALL}"
if [[ "${SUDOINSTALL}" == "yes" ]]; then
	echo "doing sudo install of keyman-config"
	make install
elif [[ "${SUDOINSTALL}" == "uninstall" ]]; then
	echo "doing sudo uninstall of keyman-config"
	make uninstall
else
	echo "doing /tmp install of keyman-config"
	make install-temp
fi
cd $BASEDIR

if [ -f "/usr/share/doc/ibus-kmfl/kmfl.xml" ] && [ "${SUDOINSTALL}" == "uninstall" ]; then
	echo "putting ibus-kmfl package component file back"
	sudo mv /usr/share/doc/ibus-kmfl/kmfl.xml /usr/share/ibus/component/
fi

