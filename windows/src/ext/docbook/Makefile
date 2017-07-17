# $Id: Makefile.tests 7519 2007-10-10 16:56:54Z xmldoc $
#
# This makefile does a "smoketest" of stylesheets for various
# output formats in the DocBook XSL Stylesheets release package.
# It doesn't actually check the output -- it's just useful for
# confirming whether each XSLT transformation actually executes
# successfully without any errors.
#
# To use it, run "make check" or just "make"

XSLTPROC=xsltproc
XSLTPROC_FLAGS=

TESTFILE=tests/refentry.007.xml
TESTFILE=tests/refentry.007.ns.xml

NORMAL_STYLES=fo/docbook.xsl html/docbook.xsl xhtml/docbook.xsl
NORMAL_PROFILE_STYLES=fo/profile-docbook.xsl html/profile-docbook.xsl xhtml/profile-docbook.xsl
CHUNK_STYLES=html/chunk.xsl html/onechunk.xsl xhtml/chunk.xsl xhtml/onechunk.xsl
HELP_STYLES=htmlhelp/htmlhelp.xsl javahelp/javahelp.xsl eclipse/eclipse.xsl
MULTIFILE_STYLES=$(CHUNK_STYLES) $(HELP_STYLES)
CHUNK_PROFILE_STYLES=html/profile-chunk.xsl html/profile-onechunk.xsl xhtml/profile-chunk.xsl xhtml/profile-onechunk.xsl
HELP_PROFILE_STYLES=htmlhelp/profile-htmlhelp.xsl eclipse/profile-eclipse.xsl javahelp/profile-javahelp.xsl
MULTIFILE_PROFILE_STYLES=$(CHUNK_PROFILE_STYLES) $(HELP_PROFILE_STYLES)

MAN_STYLE=manpages/docbook.xsl
MAN_PROFILE_STYLE=manpages/profile-docbook.xsl

ROUNDTRIP_STYLES=roundtrip/dbk2ooo.xsl roundtrip/dbk2pages.xsl roundtrip/dbk2wordml.xsl
SLIDES_STYLES=slides/html/default.xsl slides/xhtml/default.xsl slides/fo/plain.xsl
WEBSITE_STYLES=website/website.xsl
WEBSITE_CHUNK_STYLES=website/chunk-website.xsl

# chunked output gets written to TMP_OUTPUT_DIR
TMP_OUTPUT_DIR=/tmp/smoketest-output/
# if you don't want TMP_OUTPUT_DIR and its contents deleted, unset
# SMOKETEST_CLEAN_TARGET; e.g. "make check SMOKETEST_CLEAN_TARGET=''"
SMOKETEST_CLEAN_TARGET=smoketest-clean

check: smoketest-make-tmp-dir smoketest-normal smoketest-normal-profile smoketest-chunk smoketest-chunk-profile smoketest-man smoketest-man-profile $(SMOKETEST_CLEAN_TARGET)

smoketest-make-tmp-dir:
	$(RM) -r $(TMP_OUTPUT_DIR)
	mkdir '$(TMP_OUTPUT_DIR)'

smoketest-normal:
	for stylesheet in $(NORMAL_STYLES); do \
	echo "$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE) > /dev/null"; \
	$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE) > /dev/null; \
	echo "$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE-NS) > /dev/null"; \
	$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE-NS) > /dev/null; \
	done

smoketest-normal-profile:
	for stylesheet in $(NORMAL_PROFILE_STYLES); do \
	echo "$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE) > /dev/null"; \
	$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE) > /dev/null; \
	echo "$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE-NS) > /dev/null"; \
	$(XSLTPROC) $(XSLTPROC_FLAGS) $$stylesheet $(TESTFILE-NS) > /dev/null; \
	done

smoketest-chunk:
	for stylesheet in $(MULTIFILE_STYLES) ; do \
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam base.dir $(TMP_OUTPUT_DIR) --param manifest.in.base.dir 1 $$stylesheet $(TESTFILE); \
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam base.dir $(TMP_OUTPUT_DIR) --param manifest.in.base.dir 1 $$stylesheet $(TESTFILE-NS); \
	done;

smoketest-chunk-profile:
	for stylesheet in $(MULTIFILE_PROFILE_STYLES) ; do \
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam base.dir $(TMP_OUTPUT_DIR) --param manifest.in.base.dir 1 $$stylesheet $(TESTFILE); \
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam base.dir $(TMP_OUTPUT_DIR) --param manifest.in.base.dir 1 $$stylesheet $(TESTFILE-NS); \
	done;

smoketest-man:
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam man.output.base.dir $(TMP_OUTPUT_DIR) --param man.output.in.separate.dir 1 $(MAN_STYLE) $(TESTFILE); \
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam man.output.base.dir $(TMP_OUTPUT_DIR) --param man.output.in.separate.dir 1 $(MAN_STYLE) $(TESTFILE-NS); \

smoketest-man-profile:
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam man.output.base.dir $(TMP_OUTPUT_DIR) --param man.output.in.separate.dir 1 $(MAN_PROFILE_STYLE) $(TESTFILE); \
	$(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam man.output.base.dir $(TMP_OUTPUT_DIR) --param man.output.in.separate.dir 1 $(MAN_PROFILE_STYLE) $(TESTFILE-NS); \

smoketest-clean:
	$(RM) -r $(TMP_OUTPUT_DIR)
