## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

AM_CPPFLAGS =\
    -I$(top_srcdir)/include 

kmflcomp_CFLAGS =\
	 -Wall\
	 -g

bin_PROGRAMS = kmflcomp

kmflcomp_SOURCES = \
	kmfl_compiler.c

kmflcomp_LDFLAGS = -lX11 -L/usr/X11R6/lib -Wl,--no-as-needed

kmflcomp_LDADD = ../src/.libs/libkmflcomp.a
