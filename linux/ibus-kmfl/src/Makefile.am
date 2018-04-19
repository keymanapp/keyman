# vim:set noet ts=4:
#
# ibus-anthy - The Anthy engine for IBus
#
# Copyright (c) 2009 Doug Rintoul <doug_rintoul@sil.org>
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
	-DPKGDATADIR=\"$(pkgdatadir)\" \
	$(NULL)

AM_LDFLAGS = \
	@IBUS_LIBS@ \
	-lkmfl -lX11 \
	$(NULL)

check_PROGRAMS = \
	test-kmfl \
	$(NULL)

TESTS = \
	$(check_PROGRAMS) \
	$(NULL)


test_kmfl_SOURCES = \
	kmflutil.c \
	$(NULL)
test_kmfl_CFLAGS = \
	$(AM_CFLAGS) \
	-DDEBUG \
	$(NULL)

libexec_PROGRAMS = ibus-engine-kmfl

ibus_engine_kmfl_SOURCES = \
	kmflutil.c \
	kmflutil.h \
	main.c \
	engine.c \
	engine.h \
	$(NULL)
ibus_engine_kmfl_CFLAGS = \
	@IBUS_CFLAGS@ \
	-DPKGDATADIR=\"$(pkgdatadir)\" \
	$(NULL)

ibus_engine_kmfl_LDFLAGS = \
	@IBUS_LIBS@ \
	-lkmfl -lX11 \
	$(NULL)

component_DATA = \
	kmfl.xml \
	$(NULL)
componentdir = @datadir@/ibus/component

EXTRA_DIST = \
	kmfl.xml.in \
	$(NULL)

CLEANFILES = \
	kmfl.xml \
	$(NULL)

kmfl.xml: kmfl.xml.in
	( \
		libexecdir=${libexecdir}; \
		pkgdatadir=${pkgdatadir}; \
		s=`cat $<`; \
		eval "echo \"$${s}\""; \
	) > $@

test: ibus-engine-kmfl
	$(builddir)/ibus-engine-kmfl
