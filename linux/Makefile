#!/usr/bin/make

default: # builds only
	BUILDONLY="yes" ./scripts/build.sh

clean:
	rm -rf build-*

reconf: clean
	./scripts/reconf.sh

configure:
	CONFIGUREONLY="yes" ./scripts/build.sh

fullbuild: # configure and build
	./scripts/build.sh

tmpinstall: reconf
	INSTALLDIR="/tmp/keyman" ./scripts/build.sh
	INSTALLDIR="/tmp/keyman" ./scripts/install.sh

install: # run as sudo
	SUDOINSTALL="yes" ./scripts/install.sh

uninstall: # run as sudo
	SUDOINSTALL="uninstall" ./scripts/install.sh

sources:
	./scripts/dist.sh

tmpsources:
	PKG_CONFIG_PATH="../../core/build/arch/release/meson-private" ./scripts/dist.sh

origdist:
	./scripts/dist.sh origdist

sourcepackage: reconf origdist
	./scripts/deb.sh

nightlydist: reconf sources
	echo "Built nightly source release in dist/"

check:
	cd keyman-config && ./run-tests.sh
	cd ibus-keyman && ./build.sh test

tmpcheck: check

tests: check
