## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

SCIM_MAJOR=@SCIM_MAJOR@
		   
SCIM_MINOR=@SCIM_MINOR@

SCIM_BINARY_VERSION=@SCIM_BINARY_VERSION@

INCLUDES =\
	 -I$(includedir)/scim-1.0

kmfl_la_CFLAGS =\
	-DSCIM_ICONDIR=\"$(datadir)/scim/icons\"\
	-DSCIM_$(SCIM_MAJOR)_$(SCIM_MINOR)_0\
	-DSCIM_KMFL_IMENGINE_MODULE_DATADIR=\"$(datadir)/scim/kmfl\" \
	 -Wall\
	 -O1\
	 -g

kmfl_la_CXXFLAGS =\
	-DSCIM_ICONDIR=\"$(datadir)/scim/icons\"\
	-DSCIM_$(SCIM_MAJOR)_$(SCIM_MINOR)_0\
	-DSCIM_KMFL_IMENGINE_MODULE_DATADIR=\"$(datadir)/scim/kmfl\" \
	 -Wall\
	 -O1\
	 -g

moduledir		= $(libdir)/scim-1.0/1.0.0/IMEngine
module_LTLIBRARIES	= kmfl.la

kmfl_la_SOURCES = \
	scim_kmfl_imengine.cpp \
	xkbmap.cpp

kmfl_la_LDFLAGS = \
	-L/usr/X11R6/lib -avoid-version -rpath $(libdir)/scim-1.0/1.0.0/IMEngine  -module

kmfl_la_LIBADD = \
	 -lkmfl -lxkbfile -lX11
