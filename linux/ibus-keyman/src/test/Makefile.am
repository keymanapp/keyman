# vim:set noet ts=4:
#
# ibus-keyman - The Keyman engine for IBus
#
# Copyright (c) 2021 SIL International
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

include $(top_srcdir)/glib-tap.mk

AUTOMAKE_OPTIONS = subdir-objects

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

uninstalled_test_scripts = run-tests.sh

libexec_PROGRAMS = \
  keymanutil-tests \
  $(NULL)

keymanutil_tests_SOURCES = \
  keymanutil_tests.c \
  ../keymanutil.c \
  ../keymanutil.h \
  ../kmpdetails.c \
  ../kmpdetails.h \
  $(NULL)

keymanutil_tests_CFLAGS = \
  @IBUS_CFLAGS@ \
  @GTK_CFLAGS@ \
  @X11_CFLAGS@ \
  @JSON_GLIB_CFLAGS@ \
  @KEYMAN_PROC_CFLAGS@ \
  -DPKGDATADIR=\"$(pkgdatadir)\" \
  -I$(top_srcdir)/src \
  -I$(top_builddir)/src \
  $(NULL)

keymanutil_tests_CPPFLAGS = \
  @IBUS_CFLAGS@ \
  @GTK_CFLAGS@ \
  @X11_CFLAGS@ \
  @JSON_GLIB_CFLAGS@ \
  @KEYMAN_PROC_CFLAGS@ \
  -DPKGDATADIR=\"$(pkgdatadir)\" \
  -I$(top_srcdir)/src \
  -I$(top_builddir)/src \
  $(NULL)

keymanutil_tests_LDFLAGS = \
  @GTK_LIBS@ \
  @IBUS_LIBS@ \
  @X11_LIBS@ \
  @JSON_GLIB_LIBS@ \
  @KEYMAN_PROC_LIBS@ \
  $(NULL)
