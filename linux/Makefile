#!/usr/bin/make

tmpbuild: devreconf
	./scripts/build.sh

install_local: devreconf
	SUDOINSTALL="yes" ./scripts/build.sh

uninstall_local: devreconf
	SUDOINSTALL="uninstall" ./scripts/build.sh

debnocow:
	./scripts/deb.sh

reconf:
	./scripts/reconf.sh

dist:
	./scripts/dist.sh

origdist:
	./scripts/dist.sh origdist

sourcepackage: reconf origdist
	./scripts/deb.sh sourcepackage

devsourcepackage: devreconf origdist
	./scripts/deb.sh sourcepackage

devreconf:
	./scripts/reconf.sh dev

cow:
	./scripts/cow.sh

deb: devreconf origdist cow debnocow
	echo "Built dev Debian packages in builddebs/"

releasedeb: reconf origdist cow debnocow
	echo "Built release Debian packages in builddebs/"

nightlydist: devreconf dist
	echo "Built nightly source release in dist/"
