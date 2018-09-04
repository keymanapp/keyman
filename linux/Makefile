#!/usr/bin/make

tmpbuild: devreconf
	./build.sh

install_local: devreconf
	SUDOINSTALL="yes" ./build.sh

uninstall_local: devreconf
	SUDOINSTALL="uninstall" ./build.sh

debnocow:
	./deb.sh

reconf:
	./reconf.sh

dist:
	./dist.sh

origdist:
	./dist.sh origdist

sourcepackage:
	./deb.sh sourcepackage

devreconf:
	./reconf.sh dev

cow:
	./cow.sh

deb: devreconf origdist cow debnocow
	echo "Built dev Debian packages in builddebs/"

releasedeb: reconf origdist cow debnocow
	echo "Built release Debian packages in builddebs/"

nightlydist: devreconf dist