#!/usr/bin/make

tmpbuild:
	./build.sh

install_local:
	SUDOINSTALL="yes" ./build.sh

uninstall_local:
	SUDOINSTALL="uninstall" ./build.sh

deb: cow
	./deb.sh

cow:
	./cow.sh