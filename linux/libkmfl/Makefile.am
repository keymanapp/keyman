## Process this file with automake to produce Makefile.in

SUBDIRS = include src
ACLOCAL_AMFLAGS = -I m4
libkmfldocdir = ${docdir}
libkmfldoc_DATA = \
	README\
	COPYING\
	AUTHORS\
	ChangeLog\
	INSTALL\
	NEWS

EXTRA_DIST = $(libkmfldoc_DATA) VERSION

# Copy all the spec files. Of cource, only one is actually used.
dist-hook:
	for specfile in *.spec; do \
		if test -f $$specfile; then \
			cp -p $$specfile $(distdir); \
		fi \
	done

