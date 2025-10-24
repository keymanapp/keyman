#!/usr/bin/make

default:
	echo "Use ./build.sh command instead of make!"

version_reconf:
	cd .. && ./scripts/reconf.sh

version: version_reconf
	$(eval KEYMAN_VERSION := $(shell ./build.sh build && python3 -c "from keyman_config import __releaseversion__; print(__releaseversion__)"))

# i18n

POFILES := $(shell find locale -name \*.po)
MOFILES := $(POFILES:.po=/LC_MESSAGES/keyman-config.mo)

update-template: version
	xgettext --package-name "keyman" --package-version "$(KEYMAN_VERSION)" \
		--msgid-bugs-address "<support@keyman.com>" --copyright-holder "SIL Global" \
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
