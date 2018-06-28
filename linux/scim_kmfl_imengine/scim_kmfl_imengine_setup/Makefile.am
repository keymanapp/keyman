## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration
SCIM_MAJOR=@SCIM_MAJOR@
		   
SCIM_MINOR=@SCIM_MINOR@

SCIM_BINARY_VERSION=@SCIM_BINARY_VERSION@

AM_CPPFLAGS  =\
	 $(SCIM_CFLAGS)\
	 $(GTK_CFLAGS)

kmfl_imengine_setup_la_CFLAGS =\
	-DSCIM_ICONDIR=\"$(datadir)/scim/icons\"\
	-DSCIM_$(SCIM_MAJOR)_$(SCIM_MINOR)_0\
	-DSCIM_KMFL_SYSTEM_KEYBOARDS_DIR=\"$(datadir)/scim/kmfl\" \
	 -Wall\
	 -g

kmfl_imengine_setup_la_CXXFLAGS =\
	-DSCIM_ICONDIR=\"$(datadir)/scim/icons\"\
	-DSCIM_$(SCIM_MAJOR)_$(SCIM_MINOR)_0\
	-DSCIM_KMFL_SYSTEM_KEYBOARDS_DIR=\"$(datadir)/scim/kmfl\" \
	 -Wall\
	 -g

moduledir		= $(libdir)/scim-1.0/$(SCIM_BINARY_VERSION)/SetupUI
module_LTLIBRARIES	= kmfl_imengine_setup.la

kmfl_imengine_setup_la_SOURCES = \
	scim_kmfl_imengine_setup.cpp 

kmfl_imengine_setup_la_LDFLAGS = \
	-avoid-version -module

kmfl_imengine_setup_la_LIBADD =  \
	$(GTK_LIBS) \
	-lkmfl

EXTRA_DIST = kmfl_private.h
