## Makefile.am for gtk+/modules/input

INCLUDES = \
	$(GTK_CFLAGS) \
	-I./include

LDADDS = \
	$(GTK_LIBS)

moduledir = @GTK_LIBDIR@/gtk-2.0/immodules

im_xim_kmfl_la_LDFLAGS = -rpath $(moduledir) -avoid-version -module
im_xim_kmfl_la_SOURCES = 		\
	gtkimcontextxim.c 	\
	gtkimcontextxim.h	\
	imxim.c
im_xim_kmfl_la_LIBADD = $(LDADDS)

RUN_QUERY_IMMODULES_TEST=test -z "$(DESTDIR)"

# Running this if cross compiling or if DESTDIR is set is going to
# not work at all, so skip it.
# We use install-data-hook here to workaround a bug in automake and/or libtool
# that makes the install target for the loader libraries a dependency on
# install-data-am, and not install-exec-am. We need to ensure this gets run
# after the libraries are installed in their final locations.
install-data-hook:
	@if $(RUN_QUERY_IMMODULES_TEST) ; then \
	  echo $(mkinstalldirs) $(DESTDIR)$(sysconfdir)/gtk-2.0 ; \
	  $(mkinstalldirs) $(DESTDIR)$(sysconfdir)/gtk-2.0 ; \
	  echo "gtk-query-immodules-2.0 > $(DESTDIR)$(sysconfdir)/gtk-2.0/gtk.immodules" ; \
	  gtk-query-immodules-2.0 > $(DESTDIR)$(sysconfdir)/gtk-2.0/gtk.immodules ; \
	else \
	  echo "***" ; \
	  echo "*** Warning: gtk.immodules not built" ; \
	  echo "***" ; \
	  echo "*** Generate this file manually on host" ; \
	  echo "*** system using gtk-query-immodules-2.0" ; \
	  echo "***" ; \
        fi

uninstall-local:
	rm -f $(DESTDIR)$(sysconfdir)/gtk-2.0/gtk.immodules

module_LTLIBRARIES =				\
	im-xim-kmfl.la			

gtk.immodules: Makefile.am $(module_LTLIBRARIES)
	gtk-query-immodules-2.0 $(module_LTLIBRARIES) > gtk.immodules

CLEANFILES = gtk.immodules

all-local: gtk.immodules
