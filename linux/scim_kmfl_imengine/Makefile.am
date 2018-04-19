SUBDIRS = po src scim_kmfl_imengine_setup data

ACLOCAL_AMFLAGS = -I m4

# Copy all the spec files. Of cource, only one is actually used.
dist-hook:
	for specfile in *.spec; do \
		if test -f $$specfile; then \
			cp -p $$specfile $(distdir); \
		fi \
	done

