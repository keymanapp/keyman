#!/usr/bin/make

default:
	./build.sh clean build

install:
	./build.sh install

uninstall: # run as sudo
	./build.sh uninstall

clean:
	./build.sh clean

check:
	./build.sh test

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
	cd .. && ./scripts/reconf.sh

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
