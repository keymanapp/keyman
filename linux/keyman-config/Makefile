#!/usr/bin/make

VERSION=10.99.1

clean:
	-rm -rf dist make_deb build

dist: clean
	python3 setup.py sdist

deb: clean dist
	-mkdir make_deb
	cd make_deb && tar xf ../dist/keyman_config-$(VERSION).tar.gz && \
		mv keyman_config-$(VERSION) keyman-config-$(VERSION) && \
		tar cfz keyman-config_$(VERSION).orig.tar.gz keyman-config-$(VERSION) && \
		cd keyman-config-$(VERSION) && cp -a ../../debian .
	cd make_deb/keyman-config-$(VERSION) && debuild -us -uc

man:
	help2man -N ./get_kmp -o debian/man/get_kmp.1
	help2man -N ./install_kmp -o debian/man/install_kmp.1
	help2man -N ./kvk2ldml -o debian/man/kvk2ldml.1
	help2man -N ./list_installed_kmp -o debian/man/list_installed_kmp.1
	help2man -N ./uninstall_kmp -o debian/man/uninstall_kmp.1

