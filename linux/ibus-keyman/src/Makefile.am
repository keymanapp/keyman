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

SUBDIRS = \
  test \
  $(NULL)

AM_CFLAGS = \
	@IBUS_CFLAGS@ \
	@GTK_CFLAGS@ \
	@JSON_GLIB_CFLAGS@ \
	@X11_CFLAGS@ \
	@KEYMAN_PROC_CFLAGS@ \
	-DPKGDATADIR=\"$(pkgdatadir)\" \
	$(NULL)

AM_LDFLAGS = \
	@IBUS_LIBS@ \
	@GTK_LIBS@ \
	@X11_LIBS@ \
	@JSON_GLIB_LIBS@ \
	@KEYMAN_PROC_LIBS@ \
	$(NULL)

check_PROGRAMS = \
	print_kmpdetails \
	print_kmp \
	$(NULL)

TESTS = \
	test/print_kmpdetails.test \
	test/print_kmp.test \
	$(NULL)

print_kmp_SOURCES = \
	test/print_kmp.c

print_kmpdetails_SOURCES = \
	kmpdetails.c \
	test/print_kmpdetails.c \
	$(NULL)
print_kmpdetails_CFLAGS = \
	$(AM_CFLAGS) \
	@JSON_GLIB_CFLAGS@ \
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
	keycodes.h \
	keyman-service.c \
	keyman-service.h \
	keyman-version.h \
	$(NULL)

ibus_engine_keyman_CFLAGS = \
	@IBUS_CFLAGS@ \
	@GTK_CFLAGS@ \
	@X11_CFLAGS@ \
	@JSON_GLIB_CFLAGS@ \
	@KEYMAN_PROC_CFLAGS@ \
	-DPKGDATADIR=\"$(pkgdatadir)\" \
	$(NULL)

ibus_engine_keyman_LDFLAGS = \
	@GTK_LIBS@ \
	@IBUS_LIBS@ \
	@X11_LIBS@ \
	@JSON_GLIB_LIBS@ \
	@KEYMAN_PROC_LIBS@ \
	$(NULL)

component_DATA = \
	keyman.xml \
	$(NULL)
componentdir = @datadir@/ibus/component

EXTRA_DIST = \
	keyman.xml.in \
	test/print_kmpdetails.test \
	test/print_kmp.test \
	test/kmp.json \
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
