#!/usr/bin/make

default:
	echo "Use ./build.sh command instead of make!"

version:
	$(eval KEYMAN_VERSION := $(shell ./build.sh build > /dev/null && python3 -c "from keyman_config import __releaseversion__; print(__releaseversion__)"))

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

create-test-po:
	cd locale && \
		msginit --locale=TEST --width=98 --no-translator --input keyman-config.pot --output-file=TEST.po && \
		mv TEST.po TEST1.po && \
		../../scripts/create-test-po.py TEST1.po TEST.po && \
		rm TEST1.po

test-po: create-test-po
	mkdir -p locale/TEST/LC_MESSAGES
	msgfmt -v --check --output-file=locale/TEST/LC_MESSAGES/keyman-config.mo locale/TEST.po
	TEXTDOMAINDIR=./locale LANGUAGE=TEST ./km-config
