# vim:set noet ts=4:
#
# ibus-keyman - The Keyman engine for IBus
#
# Copyright (c) 2018 SIL International
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

AM_CFLAGS = \
	@IBUS_CFLAGS@ \
	@JSON_GLIB_CFLAGS@ \
	@KEYMAN_PROC_CFLAGS@ \
	-DPKGDATADIR=\"$(pkgdatadir)\" \
	$(NULL)

AM_LDFLAGS = \
	@IBUS_LIBS@ \
	@JSON_GLIB_LIBS@ \
	@KEYMAN_PROC_LIBS@ \
	-lX11 \
	$(NULL)

check_PROGRAMS = \
	test-keyman \
	$(NULL)

TESTS = \
	$(check_PROGRAMS) \
	$(NULL)


test_keyman_SOURCES = \
	keymanutil.c \
	$(NULL)
test_keyman_CFLAGS = \
	$(AM_CFLAGS) \
	-DDEBUG \
	$(NULL)

libexec_PROGRAMS = ibus-engine-keyman

ibus_engine_keyman_SOURCES = \
	keymanutil.c \
	keymanutil.h \
	kmpdetails.c \
	kmpdetails.h \
	main.c \
	engine.c \
	engine.h \
	keyman-service.c \
	keyman-service.h \
	$(NULL)

ibus_engine_keyman_CFLAGS = \
	@IBUS_CFLAGS@ \
	@JSON_GLIB_CFLAGS@ \
	@KEYMAN_PROC_CFLAGS@ \
	-DPKGDATADIR=\"$(pkgdatadir)\" \
	$(NULL)

ibus_engine_keyman_LDFLAGS = \
	@IBUS_LIBS@ \
	@JSON_GLIB_LIBS@ \
	@KEYMAN_PROC_LIBS@ \
	-lX11 \
	$(NULL)

component_DATA = \
	keyman.xml \
	$(NULL)
componentdir = @datadir@/ibus/component

EXTRA_DIST = \
	keyman.xml.in \
	$(NULL)

CLEANFILES = \
	keyman.xml \
	$(NULL)

keyman.xml: keyman.xml.in
	( \
		libexecdir=${libexecdir}; \
		pkgdatadir=${pkgdatadir}; \
		s=`cat $<`; \
		eval "echo \"$${s}\""; \
	) > $@

test: ibus-engine-keyman
	$(builddir)/ibus-engine-keyman
