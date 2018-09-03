#!/usr/bin/make

tmpbuild:
	./build.sh

install_local:
	SUDOINSTALL="yes" ./build.sh

uninstall_local:
	SUDOINSTALL="uninstall" ./build.sh

debnocow:
	./deb.sh

deb: cow debnocow

cow:
	./cow.sh