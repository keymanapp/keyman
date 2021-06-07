#!/bin/bash

# Install keyboardprocessor, kmflcomp, libkmfl, ibus-kmfl, ibus-keyman and keyman-config

# It must be run from the keyman/linux directory

# To install the ibus-kmfl xml file and icons to /usr/share so that ibus
# will use ibus-kmfl you must run SUDOINSTALL="yes" ./scripts/build.sh
# This will install everything else to /usr/local

set -e

BASEDIR=`pwd`

SUDOINSTALL=${SUDOINSTALL:-"no"}

INSTALLDIR=${INSTALLDIR:-"/usr/local"}

if [[ "${SUDOINSTALL}" != "no" ]]; then
	if [ "$EUID" -ne 0 ]
	then
		echo "Please run 'make un/install' with sudo"
	exit
	fi
fi

if [ -f "/usr/share/ibus/component/kmfl.xml" ] && [ "${SUDOINSTALL}" == "yes" ]; then
	if grep -Fq "/usr/lib/ibus" /usr/share/ibus/component/kmfl.xml
	then
		echo "component file is in ibus-kmfl package version so move it"
		echo "run 'sudo make uninstall' to put it back"
		mv /usr/share/ibus/component/kmfl.xml /usr/share/doc/ibus-kmfl/
	else
		echo "component file is local one so overwrite it"
	fi
fi

if [ -f "/usr/share/ibus/component/keyman.xml" ] && [ "${SUDOINSTALL}" == "yes" ]; then
	if grep -Fq "/usr/lib/ibus" /usr/share/ibus/component/keyman.xml
	then
		echo "component file is in ibus-keyman package version so move it"
		echo "run 'sudo make uninstall' to put it back"
		mv /usr/share/ibus/component/keyman.xml /usr/share/doc/ibus-keyman/
	else
		echo "component file is local one so overwrite it"
	fi
fi


cd keyboardprocessor/arch/release
ninja install
cd $BASEDIR

for proj in kmflcomp libkmfl ibus-kmfl ibus-keyman; do
	cd build-$proj
	if [[ "${SUDOINSTALL}" == "uninstall" ]]; then
		if [ ! -f Makefile ]; then
			echo "$proj must be configured before it is uninstalled. First run 'make configure'"
			exit 1
		fi
		echo "doing make uninstall of $proj"
		make uninstall
	else
		if [ ! -f Makefile ]; then
			echo "$proj must be configured and built before it is installed. First run 'make' or 'make tmpinstall'"
			exit 1
		fi
		echo "doing make install of $proj"
		make install
	fi
	cd $BASEDIR
done

cd keyman-config
echo "SUDOINSTALL: ${SUDOINSTALL}"
if [[ "${SUDOINSTALL}" == "yes" ]]; then
	if [ ! -d build ]; then
		echo "keyman-config must be built before it is installed. Run 'make configure' if needed then 'make'"
		exit 1
	fi
	echo "doing sudo glib-compile-schemas for keyman-config"
	cp com.keyman.gschema.xml /usr/share/glib-2.0/schemas/
	glib-compile-schemas /usr/share/glib-2.0/schemas/
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
	mv /usr/share/doc/ibus-kmfl/kmfl.xml /usr/share/ibus/component/
fi

if [ -f "/usr/share/doc/ibus-keyman/keyman.xml" ] && [ "${SUDOINSTALL}" == "uninstall" ]; then
	echo "putting ibus-keyman package component file back"
	mv /usr/share/doc/ibus-keyman/keyman.xml /usr/share/ibus/component/
fi
