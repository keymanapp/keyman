#!/usr/bin/make

VERSION=10.99.1

default:
	python3 setup.py build

install:
	python3 setup.py install

install-local:
	mkdir -p /tmp/kmfl/lib/python3.6/site-packages
	PYTHONUSERBASE=/tmp/kmfl python3 setup.py install --user


clean:
	-rm -rf dist make_deb build

devdist:
	python3 setup.py egg_info -b.`TZ=UTC git log -1 --pretty=format:%cd --date=format-local:%Y%m%d%H%M` sdist

dist: clean
	python3 setup.py sdist

deb: clean devdist man
	-mkdir make_deb
	cd make_deb && tar xf ../dist/keyman_config-$(VERSION).tar.gz && \
		mv keyman_config-$(VERSION) keyman-config-$(VERSION) && \
		tar cfz keyman-config_$(VERSION).orig.tar.gz keyman-config-$(VERSION) && \
		cd keyman-config-$(VERSION) && cp -a ../../debian .
	cd make_deb/keyman-config-$(VERSION) && debuild -us -uc

man:
	mkdir -p debian/man
	help2man -N ./km-package-get -o debian/man/km-package-get.1 -n "Download a Keyman keyboard package" -I maninc/km-package-get.inc
	help2man -N ./km-package-install -o debian/man/km-package-install.1 -n "Install a Keyman keyboard package" -I maninc/km-package-install.inc
	help2man -N ./km-config -o debian/man/km-config.1 -n "Keyman keyboards installation and information" -I maninc/km-config.inc
	help2man -N ./km-kvk2ldml -o debian/man/km-kvk2ldml.1 -n "Convert a Keyman on-screen keyboard file to LDML" -I maninc/km-kvk2ldml.inc
	help2man -N ./km-package-list-installed -o debian/man/km-package-list-installed.1 -n "List installed Keyman keyboard packages" -I maninc/km-package-list-installed.inc
	help2man -N ./km-package-uninstall -o debian/man/km-package-uninstall.1 -n "Uninstall a Keyman keyboard package" -I maninc/km-package-uninstall.inc

