#!/usr/bin/make

default: clean version man langtags
	python3 setup.py build

langtags:
	cd buildtools && python3 ./build-langtags.py

install: # run as sudo
	pip3 install qrcode sentry-sdk
	python3 setup.py install
	# install icons
	mkdir -p /usr/local/share/keyman/icons
	cp keyman_config/icons/* /usr/local/share/keyman/icons
	# install man pages
	mkdir -p /usr/local/share/man/man1
	cp debian/man/*.1 /usr/local/share/man/man1

install-temp:
	mkdir -p /tmp/kmfl/`python3 -c 'import sys;import os;pythonver="python%d.%d" % (sys.version_info[0], sys.version_info[1]);sitedir = os.path.join("lib", pythonver, "site-packages");print(sitedir)'`
	PYTHONUSERBASE=/tmp/kmfl python3 setup.py install --user

uninstall: clean # run as sudo
	rm -rf /usr/local/share/keyman/icons
	rm -f /usr/local/share/man/man1/km-*.1
	pip3 uninstall keyman_config
	rm -f /usr/local/bin/km-config
	rm -f /usr/local/bin/km-kvk2ldml
	rm -f /usr/local/bin/km-package-get
	rm -f /usr/local/bin/km-package-install
	rm -f /usr/local/bin/km-package-list-installed
	rm -f /usr/local/bin/km-package-uninstall

clean:
	-rm -rf dist make_deb build keyman_config/version.py *.egg-info __pycache__ \
		keyman_config/standards/lang_tags_map.py

devdist: version
	python3 setup.py egg_info -b.`TZ=UTC git log -1 --pretty=format:%cd --date=format-local:%Y%m%d%H%M` sdist

dist: clean version
	python3 setup.py sdist

deb: dist
	@VERSION=$(shell echo `basename dist/*.gz .tar.gz|cut -d "-" -f2` > /tmp/keyman_version)
	@echo VERSION is $(shell cat /tmp/keyman_version)
	@mkdir -p make_deb
	cd make_deb && tar xf ../dist/keyman_config-$(shell cat /tmp/keyman_version).tar.gz && \
		mv keyman_config-$(shell cat /tmp/keyman_version) keyman-config-$(shell cat /tmp/keyman_version) && \
		tar cfz keyman-config_$(shell cat /tmp/keyman_version).orig.tar.gz keyman-config-$(shell cat /tmp/keyman_version) && \
		cd keyman-config-$(shell cat /tmp/keyman_version) && cp -a ../../debian . && dch -v$(shell cat /tmp/keyman_version)-1 ""
	cd make_deb/keyman-config-$(shell cat /tmp/keyman_version) && debuild -us -uc
	rm /tmp/keyman_version

man:
	mkdir -p debian/man
	help2man -N ./km-package-get -o debian/man/km-package-get.1 -n "Download a Keyman keyboard package" -I maninc/km-package-get.inc
	help2man -N ./km-package-install -o debian/man/km-package-install.1 -n "Install a Keyman keyboard package" -I maninc/km-package-install.inc
	help2man -N ./km-config -o debian/man/km-config.1 -n "Keyman keyboards installation and information" -I maninc/km-config.inc
	help2man -N ./km-kvk2ldml -o debian/man/km-kvk2ldml.1 -n "Convert a Keyman on-screen keyboard file to LDML" -I maninc/km-kvk2ldml.inc
	help2man -N ./km-package-list-installed -o debian/man/km-package-list-installed.1 -n "List installed Keyman keyboard packages" -I maninc/km-package-list-installed.inc
	help2man -N ./km-package-uninstall -o debian/man/km-package-uninstall.1 -n "Uninstall a Keyman keyboard package" -I maninc/km-package-uninstall.inc

version:
	cd .. && ./scripts/reconf.sh keyman-config
	$(eval VERSION := $(shell python3 -c "from keyman_config import __releaseversion__; print(__releaseversion__)"))

# i18n

POFILES := $(shell find locale -name \*.po)
MOFILES := $(POFILES:.po=/LC_MESSAGES/keyman-config.mo)

update-template: version
	xgettext --package-name "keyman" --package-version "$(VERSION)" \
		--msgid-bugs-address "<support@keyman.com>" --copyright-holder "SIL International" \
		--language=Python --directory=. --output-dir=locale --output=keyman-config.pot \
		--add-comments=i18n: --sort-by-file --width=98 \
		keyman_config/*.py km-*

update-po: $(POFILES)

locale/%.po: locale/keyman-config.pot
	msgmerge --update --width=98 $@ $^

$(MOFILES): %/LC_MESSAGES/keyman-config.mo: %.po
	mkdir -p $(dir $@)
	msgfmt -v --check --output-file=$@ $^

compile-po: $(MOFILES)
