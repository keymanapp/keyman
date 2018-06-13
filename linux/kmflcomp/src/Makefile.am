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

libkmflcomp_la_LDFLAGS = -lX11 -L/usr/X11R6/lib -Wl,--no-as-needed

libkmflcomp_la_LIBADD = 

AM_YFLAGS = -d
