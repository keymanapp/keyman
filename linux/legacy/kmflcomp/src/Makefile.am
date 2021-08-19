## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

AM_CPPFLAGS =\
    -I$(top_srcdir)/include

libkmflcomp_la_CFLAGS =\
	 -Wall\
	 -g

lib_LTLIBRARIES = libkmflcomp.la

libkmflcomp_la_SOURCES = \
	yacc.y\
	lex.l\
	kmflcomp.c\
	memman.c\
	utfconv.c

EXTRA_DIST = compiler.h memman.h

libkmflcomp_la_LDFLAGS = -Wl,--no-as-needed -version-info 1:0:1

libkmflcomp_la_LIBADD = -L/usr/X11R6/lib -lX11

AM_YFLAGS = -d
