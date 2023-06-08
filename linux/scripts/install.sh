#!/bin/bash

# Install keyboardprocessor, ibus-keyman and keyman-config

# It must be run from the keyman/linux directory

set -e

BASEDIR=$(pwd)

SUDOINSTALL=${SUDOINSTALL:-"no"}

INSTALLDIR=${INSTALLDIR:-"/usr/local"}

if [[ "${SUDOINSTALL}" != "no" ]]; then
	if [ "$EUID" -ne 0 ]
	then
		echo "Please run 'build.sh un/install' with sudo"
	exit
	fi
fi

if [ -f "/usr/share/ibus/component/keyman.xml" ] && [ "${SUDOINSTALL}" == "yes" ]; then
	if grep -Fq "/usr/lib/ibus" /usr/share/ibus/component/keyman.xml
	then
		echo "component file is in ibus-keyman package version so move it"
		echo "run 'sudo build.sh uninstall' to put it back"
		mv /usr/share/ibus/component/keyman.xml /usr/share/doc/ibus-keyman/
	else
		echo "component file is local one so overwrite it"
	fi
fi

cd ../core/build/arch/release
ninja install
cd "$BASEDIR"

cd ibus-keyman
if [[ "${SUDOINSTALL}" == "uninstall" ]]; then
    echo "doing build.sh uninstall of ibus-keyman"
    ./build.sh uninstall
else
    echo "doing build.sh install of ibus-keyman"
    ./build.sh install
fi
cd "$BASEDIR"

cd keyman-config
echo "SUDOINSTALL: ${SUDOINSTALL}"
if [[ "${SUDOINSTALL}" == "yes" ]]; then
	if [ ! -d build ]; then
		echo "keyman-config must be built before it is installed. Run 'build.sh configure build'."
		exit 1
	fi
	echo "doing sudo glib-compile-schemas for keyman-config"
	cp resources/com.keyman.gschema.xml /usr/share/glib-2.0/schemas/
	glib-compile-schemas /usr/share/glib-2.0/schemas/
	echo "doing sudo install of keyman-config"
	./build.sh install
elif [[ "${SUDOINSTALL}" == "uninstall" ]]; then
	echo "doing sudo uninstall of keyman-config"
	./build.sh uninstall
else
	echo "doing /tmp install of keyman-config"
	./build.sh install
fi
cd "$BASEDIR"

if [ -f "/usr/share/doc/ibus-keyman/keyman.xml" ] && [ "${SUDOINSTALL}" == "uninstall" ]; then
	echo "putting ibus-keyman package component file back"
	mv /usr/share/doc/ibus-keyman/keyman.xml /usr/share/ibus/component/
fi
