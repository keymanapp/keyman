#!/usr/bin/make

VERSION=10.99.1

dist:
	-rm -rf dist
	python3 setup.py sdist

deb: dist
	-rm -rf dist_deb
	-mkdir dist_deb
	cd dist_deb && tar xf ../dist/keyman_config-$(VERSION).tar.gz && \
		mv keyman_config-$(VERSION) keyman-config-$(VERSION) && \
		tar cfz keyman-config_$(VERSION).orig.tar.gz keyman-config-$(VERSION) && \
		cd keyman-config-$(VERSION) && cp -a ../../debian .
	cd dist_deb/keyman-config-$(VERSION) && debuild -us -uc