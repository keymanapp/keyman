#!/usr/bin/make

VERSION=10.99.1

dist:
	-rm -rf dist
	python3 setup.py sdist

deb: dist
	-rm -rf dist_deb
	-mkdir dist_deb
	cp dist/keyman-config-$(VERSION).tar.gz dist_deb/keyman-config_$(VERSION).orig.tar.gz
	cd dist_deb && tar xf keyman-config_$(VERSION).orig.tar.gz && \
		cd keyman-config-$(VERSION) && cp -a ../../debian .
	cd dist_deb/keyman-config-$(VERSION) && debuild -us -uc