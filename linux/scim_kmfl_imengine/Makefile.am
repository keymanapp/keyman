SUBDIRS = intl po src scim_kmfl_imengine_setup data

EXTRA_DIST = intltool-extract.in intltool-merge.in intltool-update.in
DISTCLEANFILES = intltool-extract intltool-merge intltool-update


# Copy all the spec files. Of cource, only one is actually used.
dist-hook:
	for specfile in *.spec; do \
		if test -f $$specfile; then \
			cp -p $$specfile $(distdir); \
		fi \
	done

