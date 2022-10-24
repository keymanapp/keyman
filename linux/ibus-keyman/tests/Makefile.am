# vim:set noet ts=4:
#
# ibus-keyman unit tests
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

libexec_PROGRAMS = ibus-keyman-tests

uninstalled_test_scripts = run-tests.sh

# Note: don't use $(top_srcdir) here for the first `..` - automake <= 1.15 (Ubuntu 18.04)
# has a bug where it doesn't resolve the variable and so stores some files in the wrong
# location and can't find them later. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=13928
# Once we drop support for Ubuntu 18.04 Bionic we change this.

ibus_keyman_tests_SOURCES = \
  ibusimcontext.c \
  ibusimcontext.h \
  testfixture.cpp \
  testmodule.c \
  testmodule.h \
  ../../../core/src/debuglog.cpp \
  ../../../core/src/debuglog.h \
  ../../../core/src/jsonpp.cpp \
  ../../../core/src/keyboard.cpp \
  ../../../core/src/kmx/kmx_actions.cpp \
  ../../../core/src/kmx/kmx_actions.h \
  ../../../core/src/kmx/kmx_base.h \
  ../../../core/src/kmx/kmx_capslock.cpp \
  ../../../core/src/kmx/kmx_consts.cpp \
  ../../../core/src/kmx/kmx_context.cpp \
  ../../../core/src/kmx/kmx_context.h \
  ../../../core/src/kmx/kmx_conversion.cpp \
  ../../../core/src/kmx/kmx_debugger.cpp \
  ../../../core/src/kmx/kmx_debugger.h \
  ../../../core/src/kmx/kmx_environment.cpp \
  ../../../core/src/kmx/kmx_environment.h \
  ../../../core/src/kmx/kmx_file.cpp \
  ../../../common/include/kmx_file.h \
  ../../../core/src/kmx/kmx_modifiers.cpp \
  ../../../core/src/kmx/kmx_options.cpp \
  ../../../core/src/kmx/kmx_options.h \
  ../../../core/src/kmx/kmx_processevent.cpp \
  ../../../core/src/kmx/kmx_processevent.h \
  ../../../core/src/kmx/kmx_processor.cpp \
  ../../../core/src/kmx/kmx_processor.hpp \
  ../../../core/src/kmx/kmx_xstring.cpp \
  ../../../core/src/kmx/kmx_xstring.h \
  ../../../core/src/option.cpp \
  ../../../core/src/state.cpp \
  ../../../core/src/utfcodec.cpp \
  ../../../core/tests/unit/kmx/kmx_test_source.cpp \
  ../../../core/tests/unit/kmx/kmx_test_source.hpp \
  ../src/keycodes.h \
  ../src/keymanutil.c \
  ../src/keymanutil.h \
  ../src/kmpdetails.c \
  ../src/kmpdetails.h \
  $(NULL)

ibus_keyman_tests_CFLAGS = \
  @IBUS_CFLAGS@ \
  @GTK_CFLAGS@ \
  @X11_CFLAGS@ \
  @JSON_GLIB_CFLAGS@ \
  @KEYMAN_PROC_CFLAGS@ \
  -DPKGDATADIR=\"$(pkgdatadir)\" \
  -I$(top_srcdir)/../../core/src \
  -I$(top_srcdir)/../../core/src/kmx \
  -I$(top_srcdir)/../../core/tests/unit/kmx \
  -I$(top_srcdir)/src \
  -I$(top_builddir)/src \
  $(NULL)

ibus_keyman_tests_CPPFLAGS = \
  @IBUS_CFLAGS@ \
  @GTK_CFLAGS@ \
  @X11_CFLAGS@ \
  @JSON_GLIB_CFLAGS@ \
  @KEYMAN_PROC_CFLAGS@ \
  -DPKGDATADIR=\"$(pkgdatadir)\" \
  -I$(top_srcdir)/../../core/src \
  -I$(top_srcdir)/../../core/src/kmx \
  -I$(top_srcdir)/../../core/tests/unit/kmx \
  -I$(top_srcdir)/src \
  -I$(top_builddir)/src \
  $(NULL)

ibus_keyman_tests_LDFLAGS = \
  @GTK_LIBS@ \
  @IBUS_LIBS@ \
  @X11_LIBS@ \
  @JSON_GLIB_LIBS@ \
  @KEYMAN_PROC_LIBS@ \
  $(NULL)
