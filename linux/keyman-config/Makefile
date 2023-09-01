#!/usr/bin/make

default: clean version man langtags
	python3 setup.py build

langtags:
	cd buildtools && python3 ./build-langtags.py

install: # run as sudo
	pip3 install qrcode sentry-sdk
	# eventually change this to: pip3 install .
	python3 setup.py install
	# install icons
	mkdir -p /usr/local/share/keyman/icons
	cp keyman_config/icons/* /usr/local/share/keyman/icons
	# install man pages
	mkdir -p /usr/local/share/man/man1
	cp ../../debian/man/*.1 /usr/local/share/man/man1

install-temp:
	mkdir -p /tmp/keyman/$(shell python3 -c 'import sys;import os;pythonver="python%d.%d" % (sys.version_info[0], sys.version_info[1]);sitedir = os.path.join("lib", pythonver, "site-packages");print(sitedir)')
	# when we no longer have to support old pip version (python > 3.6) change this to:
	# pip3 install --prefix /tmp/keyman .
	PYTHONUSERBASE=/tmp/keyman python3 setup.py install --user

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
	-rm -rf dist make_deb build *.egg-info keyman_config/version.py
	-find . \( -name __pycache__ -o -name keyman-config.mo \) -exec rm -rf {} +
	-[ -z "$(KEYMAN_PKG_BUILD)" ] && rm -rf keyman_config/standards/lang_tags_map.py

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
		cd keyman-config-$(shell cat /tmp/keyman_version) && cp -a ../../../debian . && dch -v$(shell cat /tmp/keyman_version)-1 ""
	cd make_deb/keyman-config-$(shell cat /tmp/keyman_version) && debuild -us -uc
	rm /tmp/keyman_version

man:
	./build-help.sh --man --no-reconf

version_reconf:
	cd .. && ./scripts/reconf.sh keyman-config

version: version_reconf
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

check:
	./run-tests.sh
