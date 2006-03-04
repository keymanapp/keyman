## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

INCLUDES =\
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

EXTRA_DIST = compiler.h utfconv.h memman.h

libkmflcomp_la_LDFLAGS = 

libkmflcomp_la_LIBADD = 

AM_YFLAGS = -d
